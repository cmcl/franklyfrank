open MidTree
open ParseTree
open ParseTreeBuilder
open ListUtils
open Utility

exception TypeError of string

type src_type = ParseTree.src_type

module ENV = Map.Make(String)
module VENV = Map.Make(struct type t = int
			      let compare = Pervasives.compare
                       end)
module HENV = Set.Make(String)

type effect_env = src_type list

type datatype_env = (src_type list * constructor_declaration list) ENV.t

type interface_env = (src_type list * command_declaration list) ENV.t

type tlhandler_env = HENV.t

type env =
  {
    tenv : src_type ENV.t;
    (** Type environment mapping variables to their types. *)
    venv : src_type VENV.t;
    (** Type variable environment mapping rigid type variables to their
	instantiations. *)
    fenv : effect_env; (** Ambient effects *)
    denv : datatype_env; (** Map datatypes to a pair consisting of parameters
			     and a list of constructors. *)
    henv : tlhandler_env; (** Top-level handler environment *)
    ienv : interface_env; (** Map effect interfaces to a pair consisting of
			      their parameters and command declarations. *)
  }

type type_sig =
  TSAmbientCmds (* Greatest lower bound for TSCmd signatures w.r.t ambient
		   effects. *)
| TSAllValues of string option
(* The top element of the lattice for value type signatures. *)
| TSBool of bool
| TSFloat of float
| TSInt of int
| TSStr of string
| TSCmd of string * int (* constructor name and arity. *)
| TSCtr of string * int (* command name and arity *)
    deriving (Show)

module type TSS = sig
  type t
  val empty : t
  (** Return an empty set. *)
  val mem : type_sig -> t -> bool
  (** Return true if the set contains the specified type signature false
      otherwise. *)
  val union : t -> t -> t
  (** Return the union of the provided sets. *)
  val add : type_sig -> t -> t
  (** Add an type signature to the set if it is not already present. *)
  val singleton : type_sig -> t
  (** Return a singleton set containing the specified element. *)
  val elements : t -> type_sig list
  (** Return the list of elements of the given set. *)
  val diff : t -> t -> t
  (** [diff t u] returns the set difference t/u w.r.t to the partial order
      defined by the type_sig datatype. *)
  val is_ambient : t -> bool
  (** [is_ambient t] is [true] iff [t] is a singleton which contains the
      [TSAmbientCmds] type signature and false otherwise. *)
  val all_cmds : t -> bool
  (** [all_cmds t] is [true] iff [t] contains only signatures of the form
      TSCmd or TSAmbientCmds and [false] otherwise. *)
  val is_empty : t -> bool
  (** [is_empty t] is [true] iff [t] is empty and [false] otherwise. *)
end

module TypeSigSet = struct
  module M = Set.Make(struct
    type t = int * type_sig
    let compare (_, tsg1) (_, tsg2) = Pervasives.compare tsg1 tsg2
  end)

  type t = 
    {
      set : M.t;
      size : int
    }

  let empty = { set = M.empty ; size = 0 }

  let mem x s =
    M.mem (0, x) s.set (* Insertion order not relevant for membership. *)

  let singleton x = { set = M.singleton (0, x); size = 0 }

  let add x s =
    if mem x s then s
    else let set = M.add (s.size, x) s.set in
	 { set ; size = s.size + 1 }

  let remove x s =
    if not (mem x s) then s
    else let set = M.remove (0, x) s.set in (* insertion order irrelevant *)
	 { set ; size = s.size - 1 }

  let elements s =
    let es = M.elements s.set in
    let f (i, _) (j, _) = Pervasives.compare i j in
    let es' = List.stable_sort f es in
    let strip_ins_order (_, tsg) = tsg in
    map strip_ins_order es'

  let union s1 s2 =
    foldl (fun s x -> add x s) empty ((elements s1) ++ (elements s2))

  let is_value tsg =
    match tsg with
    | TSBool _ | TSFloat _ | TSInt _ | TSStr _ | TSCtr _ -> true
    | _ -> false

  (** The tag is irrelevant for the type signature sets. *)
  let is_top_val =
    function  TSAllValues _ -> true
            |       _       -> false

  let contains_top_val s = List.exists is_top_val (elements s)

  let filter_set p s =
    let s' = List.filter p (elements s) in
    foldl (fun s x -> add x s) empty s'

  let remove_all_values s = filter_set (not @ is_value) s

  let remove_top_val s = filter_set (not @ is_top_val) s

  let diff s1 s2 =
    let s1' = if contains_top_val s2 then remove_all_values s1 else s1 in
    let rm s x = if is_top_val x then remove_top_val s else remove x s in
    let s1' = foldl rm s1' (elements s2) in
    s1'

  let is_ambient s =
    let es = M.elements s.set in
    if length es = 1 then snd (List.hd es) = TSAmbientCmds else false

  let all_cmds s =
    let all_cmds = function  (_, TSCmd _) -> true
                           | (_, TSAmbientCmds) -> true
			   | _ -> false in
    s.size > 0 && M.for_all all_cmds s.set

  let is_empty s = s.size = 0
end
(** Set containing type signatures. *)

let type_error msg = raise (TypeError msg)

let just_hdrs = function Mtld_handler hdr -> Some hdr | _ -> None
let just_eis t =
  match t.styp_desc with Styp_effin (ei, ts) -> Some (ei, ts) | _ -> None

(* Builtin datatypes. *)

let unit_datatype =
  let ctr = Datatype.constr_decl "Unit" (TypExp.datatype "Unit" []) in
  ([], [ctr])

(* Builtin effect interfaces. *)

let console_interface =
  let put_str = EffInterface.cmd_decl "putStr" ~args:[TypExp.str ()]
    (TypExp.datatype "Unit" []) in
  let put_str_ln = EffInterface.cmd_decl "putStrLn" ~args:[TypExp.str ()]
    (TypExp.datatype "Unit" []) in
  let get_str = EffInterface.cmd_decl "getStr" (TypExp.str ()) in
  ([], [put_str; put_str_ln; get_str])

let random_interface =
  let random = EffInterface.cmd_decl "random" ~args:[]
    (TypExp.float ()) in
  ([], [random])

(* Types of builtin functions. *)

let bind_type =
  let oes = TypExp.effect_var_set in
  let x = TypExp.returner (TypExp.fresh_rigid_tvar "x") ~effs:oes () in
  let y = TypExp.returner (TypExp.fresh_rigid_tvar "y") ~effs:oes () in
  let c = TypExp.sus_comp (TypExp.comp ~args:[x] y) in
  let f = TypExp.returner c ~effs:oes () in
  TypExp.sus_comp (TypExp.comp ~args:[x; f] y)

let gt_type =
  let oes = TypExp.effect_var_set in
  let arg = TypExp.returner (TypExp.int ()) ~effs:oes () in
  let ts = [arg; arg] in
  let r = TypExp.returner (TypExp.bool ()) ~effs:oes () in
  TypExp.sus_comp (TypExp.comp ~args:ts r)

let gtf_type =
  let oes = TypExp.effect_var_set in
  let arg = TypExp.returner (TypExp.float ()) ~effs:oes () in
  let ts = [arg; arg] in
  let r = TypExp.returner (TypExp.bool ()) ~effs:oes () in
  TypExp.sus_comp (TypExp.comp ~args:ts r)

let minus_type =
  let oes = TypExp.effect_var_set in
  let arg = TypExp.returner (TypExp.int ()) ~effs:oes () in
  let ts = [arg; arg] in
  let r = TypExp.returner (TypExp.int ()) ~effs:oes () in
  TypExp.sus_comp (TypExp.comp ~args:ts r)

let plus_type =
  let oes = TypExp.effect_var_set in
  let arg = TypExp.returner (TypExp.int ()) ~effs:oes () in
  let ts = [arg; arg] in
  let r = TypExp.returner (TypExp.int ()) ~effs:oes () in
  TypExp.sus_comp (TypExp.comp ~args:ts r)

let strcat_type =
  let oes = TypExp.effect_var_set in
  let arg = TypExp.returner (TypExp.str ()) ~effs:oes () in
  let ts = [arg; arg] in
  let r = TypExp.returner (TypExp.str ()) ~effs:oes () in
  TypExp.sus_comp (TypExp.comp ~args:ts r)

let add_builtins env =
  let tenv = ENV.add "plus" plus_type
    (ENV.add "minus" minus_type
       (ENV.add "gt" gt_type
          (ENV.add "gtf" gtf_type
	     (ENV.add "strcat" strcat_type
		(ENV.add "#bind#" bind_type env.tenv))))) in
  let denv = ENV.add "Unit" unit_datatype env.denv in
  let ienv =
    ENV.add "Random" random_interface
      (ENV.add "Console" console_interface env.ienv) in
  { env
    with
      tenv;
      denv;
      henv = HENV.add "plus" (HENV.add "gt"
                                (HENV.add "gtf"
				   (HENV.add "strcat"
				      (HENV.add "minus"
					 (HENV.add "#bind#" env.henv)))));
      ienv }

let rec type_prog prog =
  let fenv = get_main_effect_set prog in
  let env = { tenv = ENV.empty; venv = VENV.empty; fenv; denv=ENV.empty;
	      henv = HENV.empty; ienv = ENV.empty } in
  let env = add_builtins env in
  let env = foldl extend_env env prog in
  let hdrs = filter_map just_hdrs prog in
  let _ = map (type_hdr env) hdrs in
  try ENV.find "main" env.tenv, env with
  | Not_found -> type_error "There must exist a unique main function"

and get_main_effect_set prog =
  let hdrs = filter_map just_hdrs prog in
  let main_hdr = List.find (fun h -> h.mhdr_name = "main") hdrs in
  let get_effects (args, r) =
    match r.styp_desc with
    | Styp_ret (es, v) -> es
    | _                -> type_error "main does not have returner type" in
  get_effects (destruct_comp_type main_hdr.mhdr_type)

and extend_env env d =
  match d with
  | Mtld_datatype dt -> add_datatype env dt
  | Mtld_effin    ei -> add_effect_interface env ei
  | Mtld_handler  h  -> add_hdr env h

and add_datatype env dt =
  let name = dt.sdt_name in
  let ps = dt.sdt_parameters in
  let cs = dt.sdt_constructors in
  { env with denv = ENV.add name (ps, cs) env.denv }

and add_effect_interface env ei =
  let name = ei.sei_name in
  let ps = ei.sei_parameters in
  let cs = ei.sei_commands in
  { env with ienv = ENV.add name (ps, cs) env.ienv }

and add_hdr env h =
  let name = h.mhdr_name in
  let t = h.mhdr_type in
  { env with tenv = ENV.add name t env.tenv; henv = HENV.add name env.henv }

and is_top_level_hdr env name = HENV.mem name env.henv

and type_hdr env h =
  let name = h.mhdr_name in
  let t = h.mhdr_type in
  try type_clauses env t h.mhdr_defs with
  | TypeError msg -> type_error (msg ^ " in handler " ^ name)

and find_datatype env d =
  try ENV.find d env.denv with
  | Not_found -> type_error ("undefined datatype " ^ d)

and find_interface ei env =
  try ENV.find ei env.ienv with
  | Not_found -> type_error ("no such interface " ^ ei)

and find_datatype_from_ctr env k =
  let dts = ENV.bindings env.denv in
  let f (d, (ps, cs)) =
    try
      let _ = List.find (fun ctr -> ctr.sctr_name = k) cs in
      Some (d, ps)
    with
    | Not_found -> None in
  let dts = filter_map f dts in
  if length dts = 1 then List.hd dts
  else type_error ("Multiple constructors named " ^ k)

and find_ctr env k d =
  let (_, cs) = find_datatype env d in
  let ctr = try List.find (fun ctr -> ctr.sctr_name = k) cs with
            | Not_found -> type_error ("no constructor named " ^ k ^
					  " for datatype " ^ d) in
  ctr

(** Type variable instantiation procedures *)
and inst_hdr env t =
  match t.styp_desc with
  | Styp_thunk {styp_desc = Styp_comp _} -> snd (inst env t)
  |  _             -> t (* Monomorphic types *)

and inst_ctr env ctr =
  let (env, sctr_args) = map_accum inst env ctr.sctr_args in
  let (_, sctr_res) = inst env ctr.sctr_res in
  { ctr with sctr_args; sctr_res }

and inst_cmd env cmd =
  let (env, scmd_args) = map_accum inst env cmd.scmd_args in
  let (_, scmd_res) = inst env cmd.scmd_res in
  { cmd with scmd_args; scmd_res }

and inst env t =
  match t.styp_desc with
  | Styp_rtvar ("£", _) -> env, t
  | Styp_rtvar (v, n)
    -> begin
         try env, VENV.find n env.venv with
	 | Not_found ->
	   let point = Unionfind.fresh (TypExp.fresh_flexi_tvar v) in
	   let uvar = { styp_desc = Styp_ref point } in
	   let env = {env with venv=VENV.add n uvar env.venv} in
	   env, uvar
       end
  | _ -> inst_with inst env t

and inst_id env t = env, t

and inst_effect_var' env e =
  match e.styp_desc with
  | Styp_rtvar ("£", _) ->
    Debug.print "instantiating £ to %s\n" (show_types env.fenv); env.fenv
  | _ -> [e]

and inst_effect_var env t = snd (inst_with inst_id env t)

and inst_with f env t =
  match t.styp_desc with
  | Styp_ret (es, v)
    -> (* Instantiate all the effect interfaces before performing uniqueness
	  filtering. This ensures that any duplicates are not unified
	  prematurely. The particular choice of instantiating interfaces then
	  epsilon provides clearer debug output (effect var. instantiation
          uses rigids). *)
       let (env, es) = map_accum (inst_with f) env es in
       let es = List.flatten (map (inst_effect_var' env) es) in
       let es = uniq_effect_set es in
       let (env, v) = inst_with f env v in
       env, TypExp.returner v ~effs:es ()
  | Styp_effin (e, ts)
    -> let (env, ts) = map_accum (inst_with f) env ts in
       env, TypExp.effin e ~params:ts ()
  | Styp_rtvar (v, _) -> f env t
  | Styp_datatype (k, vs)
    -> let (env, vs) = map_accum (inst_with f) env vs in
       env, TypExp.datatype k vs
  | Styp_thunk c -> let (env, c) = inst_with f env c in env, TypExp.sus_comp c
  | Styp_comp (ts, r)
    -> let (env, ts) = map_accum (inst_with f) env ts in
       let (env, r) = inst_with f env r in
       env, TypExp.comp ~args:ts r
  | Styp_ftvar _ (* Will never be outside a ref *)
  | Styp_ref _ (* TODO: Possibly incorrect behaviour *)
  | Styp_eff_set _
  | Styp_bool
  | Styp_int
  | Styp_float
  | Styp_str -> env, t
  | Styp_tvar _ -> assert false

(* Create a fresh reference to a fresh flexible type variable. *)
and fresh_ref s =
  TypExp.mk (Styp_ref (Unionfind.fresh
			 (TypExp.fresh_flexi_tvar s)))

(* Create a fresh returner shape from fresh flexible type variables. *)
and fresh_returner () =
  let es = fresh_ref "es" in
  let v = fresh_ref "v" in
  TypExp.returner v ~effs:[es] ()

and strvar v n = v ^ (string_of_int n)

(** env |- res checks cc *)
and type_ccomp env res cc =
  (* Checkable computation is the only syntactic class which does not
     track the ambient effects so on entry we forget whatever effects
     were allowed by the environment. *)
  let env = { env with fenv = [] } in
  match cc, res.styp_desc with
  | Mccomp_cvalue cv, Styp_ret (es, v)
    -> let env = {env with fenv = es} in
       TypExp.returner (type_cvalue env v cv) ~effs:es ()
  | Mccomp_cvalue cv, Styp_comp ([], r)
    -> (* Suspended checkable values e.g. {x} *)
       TypExp.comp (type_ccomp env r cc)
  | Mccomp_cvalue cv, Styp_ref pt
    -> begin
         match (unbox res).styp_desc with
	 | Styp_ftvar _ ->
	   let r = fresh_returner () in
	   let t = TypExp.comp r in
	   let _ = type_ccomp env t cc in
	   unify res t
	 | _
	   -> type_error
	   (Printf.sprintf
	      "failed to typecheck checkable value %s against flexible %s"
	      (ShowMidCValue.show cv) (ShowSrcType.show res))
       end
  | Mccomp_clauses [], _ -> type_empty_clause env res
  | Mccomp_clauses cls, _ -> let _ = covers env cc res in
			     type_clauses env res cls
  | _ , _
    -> type_error (Printf.sprintf
		     "failed to typecheck checkable computation %s against %s"
		     (ShowMidCComp.show cc) (ShowSrcType.show res))

and destruct_comp_type t =
  match (unbox t).styp_desc with
  | Styp_thunk thk -> destruct_comp_type thk
  | Styp_comp (args, res) -> (args, res)
  |        _     -> type_error ("incorrect handler type was:" ^
				   (ShowSrcType.show (uniq_type t)))

and type_empty_clause env res =
  let validate_arg t =
    match t.styp_desc with
    | Styp_ret (es, v)
      -> if is_uninhabited env v then ()
	 else let msg = Printf.sprintf "%s not uninhabited"
		(ShowSrcType.show t) in
	      type_error msg
    | _ -> type_error "expected returner type for empty clause" in
  match (unbox res).styp_desc with
  | Styp_comp (ts, r) -> List.iter validate_arg ts; res
  | Styp_ftvar _
    -> let r = fresh_returner () in
       let c = fresh_ref "c" in
       let t = TypExp.comp ~args:[r] c in
       unify res t
  | _ -> type_error ("expected computation type for empty clause but was " ^
			ShowSrcType.show res)

and is_uninhabited env v =
  match (unbox v).styp_desc with
  | Styp_datatype (d, _)
    -> let (ps, ctrs) = find_datatype env d in
       length ctrs = 0
  | _ -> let msg = Printf.sprintf "expected datatype but was %s"
	   (ShowSrcType.show v) in
	 type_error msg

and type_clauses env t cls =
  match (unbox t).styp_desc with
  | Styp_ftvar _
    -> begin
         match cls with
	 | (ps,_) :: _
	   -> let xs = map (fun _ -> fresh_returner ()) ps in
	      let y = fresh_returner () in
	      (* Generate the required shape for checking against a clause. *)
	      let t' = TypExp.comp ~args:xs y in
	      (* Typecheck the clauses against newly generated flexible
		 type variables. *)
	      let _ = type_clauses env t' cls in
	      (* Unify the existing flexible against this more complex
		 flexible type variable. *)
	      unify t t'
	 | _ -> assert false
       end
  | _ -> let (ts, r) = destruct_comp_type t in
	 let _ = foldl (type_clause env ts) r cls in
	 t

and type_clause env ts r (ps, cc) =
  Debug.print "types: %s with patterns: %s\n"
    (string_of_args ", " ~bbegin:false ShowSrcType.show ts)
    (string_of_args ", " ~bbegin:false ShowPattern.show ps);
  let env = try pat_matches env ts ps with
    | TypeError s
      -> type_error (Printf.sprintf "%s when checking patterns" s) in
  try type_ccomp env r cc with
  | TypeError s -> type_error (Printf.sprintf "%s when checking comp" s)

and pat_matches env ts ps = foldl type_pattern env (zip ts ps)

and pattern_error t p =
  let msg = Printf.sprintf "pattern %s does not match expected type %s"
    (ShowPattern.show p) (ShowSrcType.show t) in
  type_error msg

and type_pattern env (t, p) =
  match t.styp_desc, p.spat_desc with
  | _, Spat_any -> env
  | Styp_ret (es, v), Spat_thunk thk
    -> if ENV.mem thk env.tenv then type_error (thk ^ " already defined")
       else { env with
              tenv = ENV.add thk (TypExp.sus_comp (TypExp.comp t)) env.tenv }
  | _, Spat_comp cp -> type_comp_pattern env (t, cp)
  (* Value patterns match value types and the underlying value in returners *)
  | Styp_datatype _, Spat_value vp
  | Styp_thunk _, Spat_value vp
  | Styp_rtvar _, Spat_value vp
  | Styp_bool, Spat_value vp
  | Styp_int, Spat_value vp
  | Styp_float, Spat_value vp
  | Styp_str, Spat_value vp
  | Styp_ref _, Spat_value vp
    -> type_value_pattern env (t, vp)
  | Styp_ret (es, v), Spat_value vp
    -> type_value_pattern env (v, vp)
  | _ , _ -> pattern_error t p

and type_comp_pattern env (t, cp) =
  match t.styp_desc, cp with
  | Styp_ret (es, v), Scpat_request (c, vs, r)
    -> let es = filter_map just_eis es in
       let es = map (fun (ei, ts) -> (ei, ts, find_interface ei env)) es in
       let msg = Printf.sprintf "command %s not handled by %s" c
	 (ShowSrcType.show t) in
       let (ei, ets, ps, cmd) = find_cmd c es msg in
       (* Instantiate the parameters of the interface and the command. *)
       let (env, ps) = map_accum inst env ps in
       let cmd = inst_cmd env cmd in
       (* Unify the instantiated parameters with the arguments provided in the
	  effect set. *)
       let _ = map (uncurry unify) (zip ets ps) in
       let ts = cmd.scmd_args in
       if length ts = length vs then
	 let env = foldl type_value_pattern env (zip ts vs) in
	 let oes = TypExp.effect_var_set in
	 let arg = cmd.scmd_res in
	 let arg = TypExp.returner arg ~effs:oes () in
	 let c = TypExp.comp ~args:[arg] t in
	 let sc = TypExp.sus_comp c in
	 { env with tenv = ENV.add r sc env.tenv }
       else
	 let msg = Printf.sprintf
	   "command %s of %s expects %d arguments, %d given" c ei (length ts)
	   (length vs) in
	 type_error msg
  | _ , _ -> pattern_error t (Pattern.cpat cp)

and find_cmd c es msg =
  match foldl (is_handled c) None es with
  | Some x -> x
  | None -> type_error msg

and is_handled c acc (ei, ts, (ps, cmds)) =
  match acc with
  | Some x -> acc (* Exit after first find: effect shadowing semantics *)
  | None -> begin
              try
		let cmd = List.find (fun cmd -> cmd.scmd_name = c) cmds in
		Some (ei, ts, ps, cmd)
	      with
	      | Not_found -> None
            end

and type_value_pattern env (t, vp) =
  match t.styp_desc, vp with
  | Styp_bool, Svpat_bool _
  | Styp_int, Svpat_int _
  | Styp_float, Svpat_float _
  | Styp_str, Svpat_str _
  | _, Svpat_any
    -> env
  | _, Svpat_var x
    -> if ENV.mem x env.tenv then type_error (x ^ " already defined")
       else (Debug.print "%s |-> %s\n" x (ShowSrcType.show t);
	     { env with tenv = ENV.add x t env.tenv })
  | Styp_datatype (d, ps), Svpat_ctr (k, vs)
    -> let ctr = unify_ctr env k (length vs) (d, ps) in
       let ts = ctr.sctr_args in
       foldl type_value_pattern env (zip ts vs)
  | Styp_ref pt, _
    -> begin
         match (unbox t).styp_desc, vp with
	 | Styp_ftvar _, Svpat_ctr (k, vs)
	   -> let (d, ps) = find_datatype_from_ctr env k in
	      let t' = TypExp.datatype d ps in
	      let _ = unify t t' in env
	 | _ , _ -> type_value_pattern env (unbox t, vp)
       end
  | _ , _ -> pattern_error t (Pattern.vpat vp) (* Shouldn't happen *)

and covers env e t =
  let error cc t =
    let msg = Printf.sprintf "%s does not cover %s"
      (ShowMidCComp.show cc) (ShowSrcType.show t) in
    type_error msg in
  let rec extract_patterns cls =
    match cls with
    | [] -> []
    | (ps, _) :: cls -> ps :: extract_patterns cls in
  let is_valid_pattern t r p = true in
  (* Coverage checking for one argument with early exit checking. *)
  let covers' r (t, ps) =
    if not r then false
    else foldl (is_valid_pattern t) false ps in
  match (unbox t).styp_desc, e with
  | Styp_comp (ts, r), Mccomp_clauses cls
    -> let pss = transpose (extract_patterns cls) in
       if length ts != length pss then error e t
       else (Debug.print "COVERS: %d (%s) vs. %d\n" (length ts)
	       (show_types ts) (length pss); foldl covers' true (zip ts pss))
  |  _ , _ -> true

and type_cvalue env res cv =
  match cv, res.styp_desc with
  | Mcvalue_ivalue iv, _
    -> Debug.print "CV: Attempting to type %s with %s\n"
        (ShowMidIValue.show iv) (ShowSrcType.show res);
       let t = type_ivalue env iv in
       Debug.print "Unifying %s with %s\n" (ShowSrcType.show res)
	 (ShowSrcType.show t);
       unify res t
  | Mcvalue_ctr (k, vs), Styp_datatype (d, ts) -> type_ctr env (k, vs) (d, ts)
  | Mcvalue_thunk cc, Styp_thunk c
    -> Debug.print "CHECKING THUNK\n"; TypExp.sus_comp (type_ccomp env c cc)
  | _ , Styp_ref pt
    -> begin
         match cv, (unbox res).styp_desc with
	 | Mcvalue_ctr (k, vs), Styp_ftvar _
	   -> let (d, ps) = find_datatype_from_ctr env k in
	      let (_, ps) = map_accum inst env ps in
	      let t = TypExp.datatype d ps in
	      let _ = type_cvalue env t cv in
	      unify res t
	 | Mcvalue_thunk cc, Styp_ftvar _
	   -> let c = fresh_ref "c" in
	      (* Generate the required shape for checking against a thunk. *)
	      let t = TypExp.sus_comp c in
	      (* Unify the existing flexible against this more complex
		 flexible type variable. *)
	      let _ = unify res t in
	      (* Typecheck the computation against newly generated flexible
		 type variables. *)
	      type_cvalue env t cv
	 | _ , _ -> type_cvalue env (unbox res) cv
       end
  | _ , _ -> type_error ("cannot check " ^ ShowMidCValue.show cv ^
			    " against " ^ ShowSrcType.show res)

and validate_ctr_use ctr d n =
  let k = ctr.sctr_name in
  let ts = ctr.sctr_args in
  if length ts != n then
    let msg = Printf.sprintf
      "constructor %s of %s expects %d arguments, %d given" k d (length ts) n
    in type_error msg

and unify_ctr env k n (d, ps) =
  let ctr = find_ctr env k d in
  let ctr = inst_ctr env ctr in
  let () = validate_ctr_use ctr d n in
  let r = ctr.sctr_res in
  let res = TypExp.datatype d ps in
  (* Perform unification of the constructor result and overall result type
     expected. *)
  let _ = unify r res in
  ctr

and type_ctr env (k, vs) (d, ps) =
  let ctr = unify_ctr env k (length vs) (d, ps) in
  let ts = ctr.sctr_args in
  (* The following map operation will blow up (raise a TypeError exception) if
     we cannot unify the argument types with the provided values. The argument
     types may have changed as a result of the above unification. *)
  Debug.print "Checking %s against %s\n"
    (string_of_args ", " ShowSrcType.show ts)
    (string_of_args ", " ShowMidCValue.show vs);
  let _ = map (fun (t, v) -> type_cvalue env t v) (zip ts vs) in
  TypExp.datatype d ps

(** env |- iv infers (type_ivalue env iv) *)
and type_ivalue env iv =
  match iv with
  | Mivalue_var v -> begin
    let t = try ENV.find v env.tenv with
      | Not_found -> type_error ("undefined identifier " ^ v) in
    (* If v is a handler, instantiate all its type variables.
       Otherwise, v is a monovar but we still need to instantiate its
       effect variable (epsilon) for the context in which it is being used. *)
    let t = if is_top_level_hdr env v then inst_hdr env t
            else inst_effect_var env t in
    Debug.print "%s has type %s in env\n" v (ShowSrcType.show (uniq_type t));
    t
                     end
  | Mivalue_cmd c -> Debug.print "COMMAND %s\n" c;
    let t = type_cmd env c in
    Debug.print "CMD instantiated to %s\n" (ShowSrcType.show (uniq_type t)); t
  | Mivalue_int _ -> TypExp.int ()
  | Mivalue_float _ -> TypExp.float ()
  | Mivalue_bool _ -> TypExp.bool ()
  | Mivalue_str _ -> TypExp.str ()
  | Mivalue_icomp ic
    -> let t = type_icomp env ic in
       begin (* Check the ambient effects agrees with returner type. *)
	 match t.styp_desc with
	 | Styp_ret (es, v)
	   -> if unify_effect_sets es env.fenv then v
	      else let msg = "Unification failure: inferable computation " ^
		     "effect set not equal to ambient" in
		   failwith msg
	 | _ -> let msg = Printf.sprintf
		  "expected returner type but type was %s"
		  (ShowSrcType.show t) in
		type_error msg
       end

and show_types ts = string_of_args ", " ~bbegin:false ShowSrcType.show ts

and type_cmd env c =
  let es = env.fenv in
  Debug.print "ambient effects are %s\n" (show_types es);
  let eis = filter_map just_eis es in
  let eis = map (fun (ei, ts) -> (ei, ts, find_interface ei env)) eis in
  let msg = Printf.sprintf "command %s not handled by ambient effects %s"
    c (show_types es) in
  (* Instantiate the command and the parameters of the interface *)
  let (ei, ts, ps, cmd) = find_cmd c eis msg in
  let (env, ps) = map_accum inst env ps in
  let cmd = inst_cmd env cmd in
  (* Unify the instantiated parameters with the actual parameters provided in
     the effect set. This will give concrete types to the command's arguments
     and result type. *)
  let _ = map (uncurry unify) (zip ts ps) in
  let oes = env.fenv in
  let args = (map (fun t -> TypExp.returner t ~effs:oes ()) cmd.scmd_args) in
  let r = TypExp.returner cmd.scmd_res ~effs:es () in
  let c = TypExp.comp ~args:args r in
  let sc = TypExp.sus_comp c in
  sc

and dct t =
  match t.styp_desc with
  | Styp_thunk thk -> Debug.print "It's a thunk\n";
    begin
      match thk.styp_desc with
      | Styp_comp (args, res)
	-> Debug.print
	"It's a comp with args length %d and res:=%s\n"
	(length args) (ShowSrcType.show res)
      |  _ -> type_error ("incorrect handler type")
    end
  | Styp_datatype _ -> Debug.print "It's a datatype\n";
  |        _     -> type_error ("incorrect handler type was:" ^
				   (ShowSrcType.show t))

and is_empty_clause cc =
  match cc with
  | Mccomp_cvalue (Mcvalue_thunk (Mccomp_clauses [])) -> true
  | _ -> false

and type_icomp env ic =
  match ic with
  | Micomp_app (iv, cs)
    -> let t = type_ivalue env iv in
       let (ts, r) = destruct_comp_type t in
       let _ = begin
                 try map (fun (t, c) -> type_ccomp env t c) (zip ts cs) with
		 | Invalid_argument _
		   -> type_error (Printf.sprintf
				    "Cannot match %s with %s: %d with %d"
				    (ShowSrcType.show t)
				    (ShowMidIComp.show ic)
				    (length ts) (length cs))
               end
       in
       if does r env.fenv then r
       else type_error "ambient effects not allowed by computation"
  | Micomp_let (x, cc1, cc2) when is_empty_clause cc2
    -> let t = type_ivalue env (Mivalue_var "#bind#") in
       let ([t1;t2], r) = destruct_comp_type t in
       let _ = type_ccomp env t1 cc1 in
       let _ =  type_ccomp env t2 cc2 in
       r
  | Micomp_let (x, cc1, cc2)
    -> (* Let is syntactic sugar for bind *)
       let t = type_ivalue env (Mivalue_var "#bind#") in
       let ([t1;_], r) = destruct_comp_type t in
       let t1 = type_ccomp env t1 cc1 in
       begin
	 match t1.styp_desc with
	 | Styp_ret (_, v)
	   -> let env' = { env with tenv = ENV.add x v env.tenv } in
	      let r = type_ccomp env' r cc2 in
	      r
	 | _ -> failwith ("expected returner type but was " ^
			     (ShowSrcType.show t1))
       end

and does r es =
  let fp_soln es es' =
    let (t,ts) = (List.hd es',List.tl es') in
    let es2 = fresh_ref "es" in
    let _ = unify_flex t (TypExp.eff_set (es2::ts)) in
    (es, es') in
  match r.styp_desc with
  | Styp_ret (es', v)
    -> Debug.print "%s DOES %s\n" (show_types es')
    (show_types es);
      let es' = uniq_effect_set es' in
      let es = uniq_effect_set es in
      (* Sub-effecting is currently broken. *)
      (*sub es' es*)
      (* Check for a recursive equation *)
      let b = length es' > 1 && is_flexible_effect_set es in
      let bs = map (not @ (occur_check (List.hd es))) es' in
      let b = b && (foldl (||) false bs) in
      let (es,es') = if b then fp_soln es es' else (es,es') in
      (* Attempt to unify the effect sets. Thus we require the computation's
         effect set to be equal to the ambient. *)
      unify_effect_sets es' es
  |         _         -> type_error "expected returner type in DOES relation"

and is_interface t =
  match (unbox t).styp_desc with
  | Styp_effin _ -> true
  | _ -> false

and get_interface t =
  match (unbox t).styp_desc with
  | Styp_effin (ei, _) -> ei
  | _ -> assert false

(* Unboxing for an effect set. *)
and unbox_singleton t =
  match (unbox t).styp_desc with
  | Styp_eff_set es -> es
  | _ -> [t]

and unbox_set es = List.flatten (map unbox_singleton es)

and sub es es' =
  let error es es' =
    let msg = Printf.sprintf "failed sub: %s <= %s"
      (show_types es) (show_types es') in
    type_error msg in
  let is_var t s =
    match (unbox t).styp_desc with
    | Styp_rtvar (s', _) when s = s' -> true
    | _ -> false in
  let is_empty_set t = is_var t "@" in
  let is_effect_var t = is_var t "£" in
  let eq' t s =
    (is_effect_var t && is_effect_var s) || cmp_eff_interface t s = 0 in
  let eq r (t, s) = r && eq' t s in
  match es with
  | [t] when is_empty_set t -> true
  | t :: _ when is_effect_var t
    -> begin
         try foldl eq true (zip es es') with
         | Invalid_argument _ -> error es es'
       end
  | t :: es when is_interface t -> let ei = get_interface t in
				   let es' = remove_first es' ei in
				   sub es es'
  | _ -> if unify_effect_sets es es' then true
         else error es es'

and remove_first es' ei =
  match es' with
  | t :: es'' when is_interface t && get_interface t = ei -> es''
  | t :: es'' -> remove_first es'' ei
  | [] -> let msg = Printf.sprintf "failed to find %s interface in %s"
	    ei (show_types es') in
	  type_error msg

and free_vars t =
  match (unbox t).styp_desc with
  | Styp_datatype (_, ts)
  | Styp_effin (_, ts)    -> List.flatten (map free_vars ts)

  | Styp_thunk t          -> free_vars t
  | Styp_ref p            -> free_vars (Unionfind.find p)

  | Styp_comp (ts, t)
  | Styp_ret (ts, t)      -> (List.flatten (map free_vars ts)) ++ free_vars t

  | Styp_eff_set ts       -> List.flatten (map free_vars ts)

  | Styp_ftvar _
  | Styp_rtvar _ -> [t]

  | Styp_bool
  | Styp_int
  | Styp_float
  | Styp_str
  | Styp_tvar _ -> []

and occur_check x t = not (List.mem x (free_vars t))

(* Perform effect set uniqueness at the type-level. Only used for debugging.*)
and uniq_type t =
  match t.styp_desc with
  | Styp_thunk { styp_desc = Styp_comp (ts, t) }
    -> TypExp.sus_comp (TypExp.comp ~args:(map uniq_type ts) (uniq_type t))
  | Styp_ret (es, v) -> TypExp.returner v ~effs:(uniq_effect_set es) ()
  | Styp_ref pt -> let t' = uniq_type (Unionfind.find pt) in
		   let p = Unionfind.fresh t' in
		   {styp_desc = Styp_ref p}
  |  _ -> t

and cmp_eff_interface x y =
  match (unbox x).styp_desc , (unbox y).styp_desc with
  | Styp_effin (ei, ps) , Styp_effin (ei', ps')
    -> let ei_cmp = String.compare ei ei' in
       let t_cmp = compare x y in
       if t_cmp = 0 then t_cmp
       else if ei_cmp = 0 then
	 (* Try to unify the parameters *)
	 try
	   let _ = unify x y in
	   let t_cmp = compare x y in
	   if t_cmp = 0 then 0
	   else (Debug.print "SOMETHING HORRIBLE WENT WRONG (%d)\n" t_cmp;
		 assert false)
	 with
	 | TypeError msg -> -1 (* Don't reorder; shadowing semantics *)
       else t_cmp
  | _ , _ -> assert false

and uniq_effect_set xs =
  let cmp x y = (*Temporary hack for effect var*)
    match (unbox x).styp_desc , (unbox y).styp_desc with
    | Styp_rtvar ("£", _) , _                   ->  -1
    | _                   , Styp_rtvar ("£", _) -> 1
    | Styp_effin (ei, ps) , Styp_effin (ei', ps')
      -> cmp_eff_interface x y
    (* Flexibles represent replacements of the effect var. Need to keep these
       at the beginning of any list since they will most likely be unified
       with a set containining the effect var later. *)
    | Styp_ftvar _ , Styp_effin _ -> -1
    | Styp_effin _   , Styp_ftvar _ -> 1
    | _                   , _
      -> let msg = Printf.sprintf "uniq_effect_set [%s] at cmp (%s, %s)"
	   (show_types xs) (ShowSrcType.show x) (ShowSrcType.show y) in
	 failwith msg in
  List.sort_uniq cmp xs

and is_flexible_effect_set xs =
  (* Flexible effect sets are created on-the-fly by the typechecker for
     anonymous handlers. *)
  match xs with
  | [t] -> begin
             match (unbox t).styp_desc with
	     | Styp_ftvar _ -> true
	     |       _      -> false
           end
  |  _  -> false

and ext_pt x =
  match x.styp_desc with
  | Styp_ref px -> px
  | _ -> type_error "failed to extract point during unification"

and unify_ftvars x y =
  let px = ext_pt x in let xt = Unionfind.find px in
  let py = ext_pt y in let yt = Unionfind.find py in
  match xt.styp_desc, yt.styp_desc with
  | Styp_ftvar _ , Styp_ftvar _ -> Unionfind.union px py; true
  | _ -> false

and unify_flex x y =
  let px = ext_pt x in
  let xt = Unionfind.find px in
  match xt.styp_desc, y.styp_desc with
  | Styp_ftvar _ , Styp_ftvar _
    -> assert false (* Handled by unify_ftvars *)
  | Styp_ftvar _ , _
    -> if occur_check xt y then (Unionfind.change px y; true) else false
  | _ , _ -> false (* Handled by unify_concrete *)

and unify' x y =
  try let _ = unify x y in true with
  | TypeError _ -> false

and unify_types xs ys =
  let f = fun acc (t, t') -> acc && unify' t t' in
  try foldl f true (zip xs ys) with
  | Invalid_argument _
    -> let msg = Printf.sprintf "failed to unify lists: %s with %s"
	 (show_types xs) (show_types ys) in
       Debug.print "%s\n" msg; false

and unify_effect_sets xs ys =
  let xs = uniq_effect_set (unbox_set xs) in
  let ys = uniq_effect_set (unbox_set ys) in
  (* May need to perform flexible effect set unification *)
  if is_flexible_effect_set xs && is_flexible_effect_set ys then
    unify_ftvars (List.hd xs) (List.hd ys)
  else if is_flexible_effect_set xs then
    unify_flex (List.hd xs) (TypExp.eff_set ys)
  else if is_flexible_effect_set ys then
    unify_flex (List.hd ys) (TypExp.eff_set xs)
  else unify_types xs ys

and unify x y =
  let unify_fail x y =
    let msg = Printf.sprintf "failed to unify: %s with %s"
      (ShowSrcType.show x) (ShowSrcType.show y) in
    type_error msg in
  let unify_concrete x y =
    Debug.print "unifying %s with %s\n" (ShowSrcType.show x)
      (ShowSrcType.show y);
    match x.styp_desc, y.styp_desc with
    | Styp_thunk t          , Styp_thunk t'      -> unify' t t'
    | Styp_rtvar (v, n)     , Styp_rtvar (v', n') ->
      Debug.print "unifying rigids: %s%d and %s%d\n" v n v' n'; n = n'
    | Styp_comp (ts, t)     , Styp_comp (ts', t')
      -> unify_types ts ts' && unify' t t'

    | Styp_ret (ts, t)      , Styp_ret (ts', t')
      -> unify_effect_sets ts ts' && unify' t t'

    | Styp_datatype (s, ts) , Styp_datatype (s', ts')
    | Styp_effin (s, ts)    , Styp_effin (s', ts')
      -> s = s' && unify_types ts ts'

    | Styp_bool             , Styp_bool
    | Styp_int              , Styp_int
    | Styp_float            , Styp_float
    | Styp_str              , Styp_str      -> true
    | _                     , _             -> unify_fail x y in
  let is_ref x = match x.styp_desc with Styp_ref _ -> true | _ -> false in
  match is_ref x, is_ref y with
  | true  , false
    -> if unify_flex x y then y
       else if unify_concrete (Unionfind.find (ext_pt x)) y then y
       else unify_fail x y
  | true  , true
    -> Debug.print "Attempting to unify %s with %s\n" (ShowSrcType.show x)
                (ShowSrcType.show y);
      if Unionfind.equivalent (ext_pt x) (ext_pt y) then
	(Debug.print "\nEq\n"; x)
       else if unify_ftvars x y then (Debug.print "\nUnion\n";x)
       else if unify_flex x (Unionfind.find (ext_pt y)) then x
       else if unify_flex y (Unionfind.find (ext_pt x)) then y
       else if unify_concrete (Unionfind.find (ext_pt x))
       	                      (Unionfind.find (ext_pt y)) then x
       else unify_fail x y
  | false , true
    -> if unify_flex y x then x
       else if unify_concrete x (Unionfind.find (ext_pt y)) then x
       else unify_fail x y
  | false , false
    -> if unify_concrete x y then x
       else unify_fail x y

let rec compute_signature env t =
  match (unbox t).styp_desc with
  | Styp_datatype (d, ts)
    -> let (_, cs) = find_datatype env d in
       let make_tsg ctr = TSCtr (ctr.sctr_name, length ctr.sctr_args) in
       let ctrs = map make_tsg cs in
       foldl (fun s c -> TypeSigSet.add c s) TypeSigSet.empty ctrs
  | Styp_thunk _ -> TypeSigSet.singleton (TSAllValues None)
  | Styp_tvar _ -> TypeSigSet.empty
  | Styp_rtvar ("£", _) -> TypeSigSet.singleton TSAmbientCmds
  | Styp_rtvar ("@", _) -> TypeSigSet.empty
  | Styp_rtvar _
    -> (* Captures all possible values. *)
       TypeSigSet.singleton (TSAllValues None)
  | Styp_ftvar _ -> TypeSigSet.empty
  | Styp_eff_set _ -> TypeSigSet.empty
  | Styp_comp (_, _) -> TypeSigSet.empty
  | Styp_ret (ts, v)
    -> let f s t = TypeSigSet.union (compute_signature env t) s in
       let s = foldl f TypeSigSet.empty ts in
       TypeSigSet.union s (compute_signature env v)
  | Styp_effin (ei, ts)
    -> let (_, cs) = find_interface ei env in
       let make_tsg cmd = TSCmd (cmd.scmd_name, length cmd.scmd_args + 1) in
       let cmds = map make_tsg cs in
       foldl (fun s c -> TypeSigSet.add c s) TypeSigSet.empty cmds
  | Styp_bool
    -> TypeSigSet.add (TSBool true)
    (TypeSigSet.add (TSBool false) TypeSigSet.empty)
  | Styp_int -> TypeSigSet.singleton (TSAllValues None)
  | Styp_float -> TypeSigSet.singleton (TSAllValues None)
  | Styp_str -> TypeSigSet.singleton (TSAllValues None)
  | _ -> TypeSigSet.empty

let rec compute_arg_types env t tsg =
  match tsg, (unbox t).styp_desc with
  | TSCtr (_, _), Styp_ret (_, v) -> compute_arg_types env v tsg
  | TSCtr (k, n), Styp_datatype (d, ps)
    -> let ctr = unify_ctr env k n (d, ps) in
       ctr.sctr_args
  | TSCmd (c, _), Styp_ret (es, v)
    -> (* TODO: Package the below into a separate function, it's repeated in
          multiple places throughout this module. *)
       let eis = filter_map just_eis es in
       let eis = map (fun (ei, ts) -> (ei, ts, find_interface ei env)) eis in
       (* Instantiate the command and the parameters of the interface *)
       let (ei, ts, ps, cmd) = find_cmd c eis "compute_arg_types defect" in
       let (env, ps) = map_accum inst env ps in
       let cmd = inst_cmd env cmd in
       (* Unify the instantiated parameters with the actual parameters
          provided in the effect set. This will give concrete types to the
          command's arguments and result type. *)
       let _ = map (uncurry unify) (zip ts ps) in
       let oes = TypExp.effect_var_set in
       let arg = TypExp.returner cmd.scmd_res ~effs:oes () in
       let c = TypExp.comp ~args:[arg] t in
       let k = TypExp.sus_comp c in
       cmd.scmd_args ++ [k]
  | TSAllValues _ , _ -> []
  | _ , _ -> print_endline (Show.show<type_sig> tsg);
             print_endline (ShowSrcType.show t); assert false
  (* | TSAmbientCmds *)
  (* | TSBool b *)
  (* | TSFloat f *)
  (* | TSInt n *)
  (* | TSStr s *)
  (*   -> assert false *)

let env_lookup x env =
  if ENV.mem x env.denv then
    let (ps, cs) = ENV.find x env.denv in
    TypExp.datatype x ps
  else if ENV.mem x env.ienv then
    let (ps, cs) = ENV.find x env.ienv in
    TypExp.effin x ~params:ps ()
  else if HENV.mem x env.henv then
    ENV.find x env.tenv
  else failwith ("No datatype, effect interface or handler '" ^ x ^ "'")
