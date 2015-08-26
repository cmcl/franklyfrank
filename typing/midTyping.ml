open MidTree
open ParseTree
open ParseTreeBuilder
open ListUtils

exception TypeError of string

module ENV = Map.Make(String)

type effect_env = src_type list

type datatype_env = (src_type list * constructor_declaration list) ENV.t

type interface_env = (src_type list * command_declaration list) ENV.t

type env =
  {
    tenv : src_type ENV.t;
    (** Type environment mapping variables to their types. *)
    fenv : effect_env; (** Ambient effects *)
    denv : datatype_env; (** Map datatypes to a pair consisting of parameters
			     and a list of constructors. *)
    ienv : interface_env; (** Map effect interfaces to a pair consisting of
			      their parameters and command declarations. *)
  }

let type_error msg = raise (TypeError msg)

let just_hdrs = function Mtld_handler hdr -> Some hdr | _ -> None
let just_eis t =
  match t.styp_desc with Styp_effin (ei, _) -> Some ei | _ -> None

(* Types of builtin functions. *)

let gt_type =
  let oes = TypExp.effect_var_set in
  let arg = TypExp.returner (TypExp.int ()) ~effs:oes () in
  let ts = [arg; arg] in
  let r = TypExp.returner (TypExp.bool ()) ~effs:oes () in
  TypExp.sus_comp (TypExp.comp ~args:ts r)

let get_builtins () =
  let tenv = ENV.add "Bool" (TypExp.bool ())
    (ENV.add "Int" (TypExp.int ()) ENV.empty) in
  let tenv = ENV.add "gt" gt_type tenv in
  tenv

let rec type_prog prog =
  let tenv = get_builtins () in
  let fenv = get_main_effect_set prog in
  let env = { tenv; fenv; denv=ENV.empty; ienv = ENV.empty } in
  let env = foldl extend_env env prog in
  let hdrs = filter_map just_hdrs prog in
  let _ = map (type_hdr env) hdrs in
  try ENV.find "main" env.tenv with
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
  { env with tenv = ENV.add name t env.tenv }

and type_hdr env h =
  let name = h.mhdr_name in
  let t = h.mhdr_type in
  try type_clauses env t h.mhdr_defs with
  | TypeError msg -> type_error (msg ^ " in handler " ^ name)

and find_datatype env d =
  try ENV.find d env.denv with
  | Not_found -> type_error ("undefined datatype " ^ d)

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

and inst env t =
  match t.styp_desc with
  | Styp_rtvar "£" -> env, t
  | Styp_rtvar v
    -> begin
         try env, ENV.find v env.tenv with
	 | Not_found ->
	   let point = Unionfind.fresh (TypExp.flexi_tvar v) in
	   let uvar = { styp_desc = Styp_ref point } in
	   let env = {env with tenv=ENV.add v uvar env.tenv} in
	   env, uvar
       end
  | _ -> inst_with inst env t

and inst_effect_var env e =
  match e.styp_desc with
  | Styp_rtvar "£" -> env.fenv
  | _ -> [e]

and inst_with f env t =
  match t.styp_desc with
  | Styp_ret (es, v)
    -> let es = List.flatten (map (inst_effect_var env) es) in
       let (env, es) = map_accum (inst_with f) env es in
       let (env, v) = inst_with f env v in
       env, TypExp.returner v ~effs:es ()
  | Styp_effin (e, ts)
    -> let (env, ts) = map_accum (inst_with f) env ts in
       env, TypExp.effin e ~params:ts ()
  | Styp_rtvar v -> f env t
  | Styp_datatype (k, vs)
    -> let (env, vs) = map_accum (inst_with f) env vs in
       env, TypExp.datatype k vs
  | Styp_thunk c -> let (env, c) = inst_with f env c in env, TypExp.sus_comp c
  | Styp_comp (ts, r)
    -> let (env, ts) = map_accum (inst_with f) env ts in
       let (env, r) = inst_with f env r in
       env, TypExp.comp ~args:ts r
  | Styp_ftvar _ (* Will never be outside a ref *)
  | Styp_ref _
  | Styp_bool
  | Styp_int -> env, t

and strvar v n = v ^ (string_of_int n)

(** env |- res checks cc *)
and type_ccomp env res cc =
  match cc, res.styp_desc with
  | Mccomp_cvalue cv, Styp_ret (es, v)
    -> let env = {env with fenv = es} in
       TypExp.returner (type_cvalue env v cv) ~effs:es ()
  | Mccomp_clauses [], _ -> type_empty_clause env res
  | Mccomp_clauses cls, _ -> type_clauses env res cls
  | _ , _
    -> type_error (Printf.sprintf
		     "failed to typecheck checkable computation %s against %s"
		     (ShowMidCComp.show cc) (ShowSrcType.show res))

and destruct_comp_type t =
  match t.styp_desc with
  | Styp_thunk thk -> destruct_comp_type thk
  | Styp_comp (args, res) -> (args, res)
  |        _     -> type_error ("incorrect handler type")

and type_empty_clause env res =
  match res.styp_desc with
  | Styp_comp ([t], r)
    -> begin
         match t.styp_desc with
	 | Styp_ret (es, v)
	   -> if is_uninhabited env v then res
    	      else let msg = Printf.sprintf "%s not uninhabited"
		     (ShowSrcType.show v) in
		   type_error msg
	 | _ -> type_error "expected returner type for empty clause"
       end
  | _ -> type_error "expected computation type for empty clause"

and is_uninhabited env v =
  match (unbox v).styp_desc with
  | Styp_datatype (d, _)
    -> let (ps, ctrs) = find_datatype env d in
       length ctrs = 0
  | _ -> let msg = Printf.sprintf "expected datatype but was %s"
	   (ShowSrcType.show v) in
	 type_error msg

(* Extract underlying type from reference. *)
and unbox t =
  match t.styp_desc with
  | Styp_ref pt -> Unionfind.find pt
  | _ -> t

and type_clauses env t cls =
  let (ts, r) = destruct_comp_type t in
  foldl (type_clause env ts) r cls

and type_clause env ts r (ps, cc) =
  Debug.print "%s with %s\n"
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
    -> { env with tenv = ENV.add thk (TypExp.sus_comp t) env.tenv }
  | _, Spat_comp cp -> type_comp_pattern env (t, cp)
(* Value patterns match value types and the underlying value in returners *)
  | Styp_datatype _, Spat_value vp
  | Styp_thunk _, Spat_value vp
  | Styp_rtvar _, Spat_value vp
  | Styp_bool, Spat_value vp
  | Styp_int, Spat_value vp
    -> type_value_pattern env (t, vp)
  | Styp_ret (es, v), Spat_value vp
    -> type_value_pattern env (v, vp)
  | _ , _ -> pattern_error t p

and type_comp_pattern env (t, cp) =
  match t.styp_desc, cp with
  | Styp_ret (es, v), Scpat_request (c, vs, r)
    -> let es = filter_map just_eis es in
       let es = map (fun ei -> (ei, ENV.find ei env.ienv)) es in
       let msg = Printf.sprintf "command %s not handled by %s" c
	 (ShowSrcType.show t) in
       let (ei, cmd) = find_cmd c es msg in
       let ts = cmd.scmd_args in
       if length ts = length vs then
	 let env = foldl type_value_pattern env (zip ts vs) in
	 let es = TypExp.closed_effect_set in
	 let arg = TypExp.returner cmd.scmd_res ~effs:es () in
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

and is_handled c acc (ei, (_, cmds)) =
  match acc with
  | Some x -> acc (* Exit after first find: effect shadowing semantics *)
  | None -> begin
              try
		let cmd = List.find (fun cmd -> cmd.scmd_name = c) cmds in
		Some (ei, cmd)
	      with
	      | Not_found -> None
            end

and type_value_pattern env (t, vp) =
  match t.styp_desc, vp with
  | Styp_bool, Svpat_bool _
  | Styp_int, Svpat_int _
  | _, Svpat_any
    -> env
  | _, Svpat_var x -> { env with tenv = ENV.add x t env.tenv }
  | Styp_datatype (d, ps), Svpat_ctr (k, vs)
    -> let ctr = find_ctr env k d in
       let () = validate_ctr_use ctr d (length vs) in
       let ts = ctr.sctr_args in
       foldl type_value_pattern env (zip ts vs)
  | _ , _ -> pattern_error t (Pattern.vpat vp) (* Shouldn't happen *)

and type_cvalue env res cv =
  match cv, res.styp_desc with
  | Mcvalue_ivalue iv, _
    -> let t = type_ivalue env iv in
       unify env res t
  | Mcvalue_ctr (k, vs), Styp_datatype (d, ts) -> type_ctr env (k, vs) (d, ts)
  | Mcvalue_thunk cc, Styp_thunk c (** TODO: Add coverage checking *)
    -> TypExp.sus_comp (type_ccomp env c cc)
  | _ , _ -> type_error ("cannot check " ^ ShowMidCValue.show cv ^
			    " against " ^ ShowSrcType.show res)

and validate_ctr_use ctr d n =
  let k = ctr.sctr_name in
  let ts = ctr.sctr_args in
  if length ts != n then
    let msg = Printf.sprintf
      "constructor %s of %s expects %d arguments, %d given" k d (length ts) n
    in type_error msg

and type_ctr env (k, vs) (d, ps) =
  let ctr = find_ctr env k d in
  let ctr = inst_ctr env ctr in
  let () = validate_ctr_use ctr d (length vs) in
  let ts = ctr.sctr_args in
  let r = ctr.sctr_res in
  let res = TypExp.datatype d ps in
  (* Perform unification of the constructor result and overall result type
     expected. *)
  let _ = unify env r res in
  (* The following map operation will blow up (raise a TypeError exception) if
     we cannot unify the argument types with the provided values. The argument
     types may have changed as a result of the above unification. *)
  Debug.print "Checking %s against %s\n"
    (string_of_args ", " ShowSrcType.show ts)
    (string_of_args ", " ShowMidCValue.show vs);
  let _ = map (fun (t, v) -> type_cvalue env t v) (zip ts vs) in
  res

(** env |- iv infers (type_ivalue env iv) *)
and type_ivalue env iv =
  match iv with
  | Mivalue_var v -> begin
    let t = try ENV.find v env.tenv with
      | Not_found -> type_error ("undefined identifier " ^ v) in
    let t = inst_hdr env t in
    Debug.print "%s has type %s in env\n" v (ShowSrcType.show t); t
                     end
  | Mivalue_cmd c -> inst_hdr env (type_cmd env c)
  | Mivalue_int _ -> TypExp.int ()
  | Mivalue_bool _ -> TypExp.bool ()
  | Mivalue_icomp ic
    -> let t = type_icomp env ic in
       begin (* Check the ambient effects agrees with returner type. *)
	 match t.styp_desc with
	 | Styp_ret (es, v)
	   -> v (* TODO: Compare es with fst (env.fenv) *)
	 | _ -> let msg = Printf.sprintf
		  "expected returner type but type was %s"
		  (ShowSrcType.show t) in
		type_error msg
       end

and show_effects es = string_of_args ", " ~bbegin:false ShowSrcType.show es

and type_cmd env c =
  let es = env.fenv in
  let eis = filter_map just_eis es in
  let eis = map (fun ei -> (ei, ENV.find ei env.ienv)) eis in
  let msg = Printf.sprintf "command %s not handled by ambient effects %s"
    c (show_effects es) in
  let (ei, cmd) = find_cmd c eis msg in
  let ts = (map (fun t -> TypExp.returner t ()) cmd.scmd_args) in
  let r = TypExp.returner cmd.scmd_res ~effs:es () in
  let c = TypExp.comp ~args:ts r in
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
  |        _     -> type_error ("incorrect handler type")

and type_icomp env ic =
  match ic with
  | Micomp_app (iv, cs)
    -> let t = type_ivalue env iv in
       Debug.print "EHEHEHEHE\n";
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
       r

and free_vars t =
  match t.styp_desc with
  | Styp_datatype (_, ts)
  | Styp_effin (_, ts)    -> List.flatten (map free_vars ts)

  | Styp_thunk t          -> free_vars t
  | Styp_ref p            -> free_vars (Unionfind.find p)

  | Styp_comp (ts, t)
  | Styp_ret (ts, t)      -> (List.flatten (map free_vars ts)) @ free_vars t

  | Styp_ftvar _
  | Styp_rtvar _ -> [t]

  | Styp_bool
  | Styp_int -> []

and occur_check x t = not (List.mem x (free_vars t))

and unify env x y =
  let unify_fail x y =
    let msg = Printf.sprintf "failed to unify: %s with %s"
      (ShowSrcType.show x) (ShowSrcType.show y) in
    type_error msg in
  let ext_pt x =
    match x.styp_desc with
    | Styp_ref px -> px
    | _ -> type_error "failed to extract point during unification" in
  let unify_ftvars x y =
    let px = ext_pt x in let xt = Unionfind.find px in
    let py = ext_pt y in let yt = Unionfind.find py in
    match xt.styp_desc, yt.styp_desc with
    | Styp_ftvar _ , Styp_ftvar _ -> Unionfind.union px py; true
    | _ -> false in
  let unify_flex x y =
    let px = ext_pt x in
    let xt = Unionfind.find px in
    match xt.styp_desc, y.styp_desc with
    | Styp_ftvar _ , Styp_ftvar _ -> assert false (* Handled by unify_ftvars *)
    | Styp_ftvar _ , _
      -> if occur_check xt y then (Unionfind.change px y; true) else false
    | _ , _ -> false (* Handled by unify_concrete *) in
  let unify_concrete x y =
    let unify' env x y =
      try let _ = unify env x y in true with
      | TypeError _ -> false in
    let unify_types xs ys =
      let f = fun acc (t, t') -> acc && unify' env t t' in
      foldl f true (zip xs ys) in
    match x.styp_desc, y.styp_desc with
    | Styp_thunk t          , Styp_thunk t' -> unify' env t t'
    | Styp_rtvar v          , Styp_rtvar v' ->
      Debug.print "unifying rigids: %s and %s" v v';
      v = v'

    | Styp_comp (ts, t)     , Styp_comp (ts', t')
    | Styp_ret (ts, t)      , Styp_ret (ts', t')
      -> unify_types ts ts' && unify' env t t'

    | Styp_datatype (s, ts) , Styp_datatype (s', ts')
    | Styp_effin (s, ts)    , Styp_effin (s', ts')
      -> s = s' && unify_types ts ts'

    | Styp_bool             , Styp_bool
    | Styp_int              , Styp_int      -> true
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
