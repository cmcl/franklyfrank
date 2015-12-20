open MidTree
open ParseTree
open ParseTreeBuilder
open ListUtils
open Utility

type mid_error =
  | Merr_not_comp of string
  | Merr_inv_clause of string
  | Merr_inv_ctr of string
  | Merr_no_main of string
  | Merr_duplicate_tvar of string
  | Merr_shadowing_builtin of string

exception Error of mid_error

let invalid_clause_error def =
  raise (Error
	   (Merr_inv_clause
	      ("Invalid pattern matching computation when parsing " ^ def)))

let invalid_constructor k def =
  raise (Error (Merr_inv_ctr
		  ("No such constructor " ^ k ^ " when parsing " ^ def)))

let not_comp k =
  raise (Error
	   (Merr_not_comp
	      (k ^ " does not have a computation type")))
  

let no_main () = raise (Error (Merr_no_main "No main function defined."))

let duplicate_tvar msg =
  raise (Error (Merr_duplicate_tvar
		  ("Duplicate type variable detected:" ^ msg)))

let shadowing_builtin msg =
  raise (Error (Merr_shadowing_builtin (msg ^ " shadows builtin.")))

module type HMS = sig
  include Map.S with type key := string
  type mt = handler_definition t
end

module type NS = sig
  type t
  val empty : t
  (** Return an empty set. *)
  val mem : string -> t -> bool
  (** Return true if the set contains the specified string false
      otherwise. *)
end

module HandlerMap = struct
  module M = Map.Make(String)
  include M
  type mt = handler_definition M.t
end

module CtrSet = struct
  module M = Set.Make(String)
  type t = M.t
  let empty = M.empty
  let mem = M.mem
end

module CmdSet = struct
  module M = Set.Make(String)
  type t = M.t
  let empty = M.empty
  let mem = M.mem
end

type prog_state =
  {
    mutable def_name : string;
    ctrs : CtrSet.t;
    cmds : CmdSet.t
  }

let just_datatype = function Sterm_datatype dt -> Some dt | _ -> None
let just_effin = function Sterm_effin ei -> Some ei | _ -> None
let just_decl = function Sterm_vdecl vd -> Some vd | _ -> None
let just_def = function Sterm_vdefn vd -> Some vd | _ -> None

let partition prog =
  let dts   = ListUtils.filter_map just_datatype prog in
  let eis   = ListUtils.filter_map just_effin prog in
  let decls = ListUtils.filter_map just_decl prog in
  let defs  = ListUtils.filter_map just_def prog in
  (dts, eis, decls, defs)

let add_ctr set ctr = CtrSet.M.add ctr.sctr_name set

let add_cmd set cmd = CmdSet.M.add cmd.scmd_name set

let add_def map def = HandlerMap.add def.mhdr_name def map

(** Functions for refining the pattern matching of handlers. *)
let rec refine_vpat st vp =
  match vp with
  | Svpat_var v (** Probe constructor environment; return ctr if found. *)
    -> if CtrSet.mem v st.ctrs then Svpat_ctr (v, []) else vp   
  | Svpat_ctr (k, ps)
    -> let ps' = List.map (refine_vpat st) ps in Svpat_ctr (k, ps')
  | Svpat_any | Svpat_int _ | Svpat_float _ | Svpat_bool _ | Svpat_str _ -> vp

let refine_cpat st cp =
  match cp with
  | Scpat_request (s, ps, k)
    -> let ps' = List.map (refine_vpat st) ps in Scpat_request (s, ps', k)

(** Using the constructor set, reconstruct the pattern lists
    to rectify any constructors incorrectly identified as variables. *)
let refine_pat st pat =
  match pat.spat_desc with
  | Spat_value vp -> Pattern.vpat(refine_vpat st vp)
  | Spat_comp cp -> Pattern.cpat(refine_cpat st cp)
  | Spat_any
  | Spat_thunk _ -> pat

(** Functions for translating the computation of a handler clause. *)

let rec translate_ccomp st cc =
  match cc with
  | CComp_cvalue cv -> Mccomp_cvalue (translate_cvalue st cv)
  | _ -> translate_clause st cc

and translate_icomp st ic =
  match ic with
  | IComp_app (iv, cs) -> let iv' = translate_ivalue st iv in
			  let cs' = List.map (translate_ccomp st) cs in
			  Micomp_app (iv', cs')
  | IComp_let (x, cc1, cc2) -> let cc1' = translate_ccomp st cc1 in
			       let cc2' = translate_ccomp st cc2 in
			       Micomp_let (x, cc1', cc2')

and translate_ivalue st iv =
  match iv with
  | IValue_ident v
    -> if CmdSet.mem v st.cmds then
	Mivalue_cmd v
      else
	Mivalue_var v
  | IValue_int n -> Mivalue_int n
  | IValue_float f -> Mivalue_float f
  | IValue_bool b -> Mivalue_bool b
  | IValue_str s -> Mivalue_str s
  | IValue_icomp ic
    -> Mivalue_icomp (translate_icomp st ic)

and tryn_make_constructor st iv =
  match iv with
  | IValue_ident v -> if CtrSet.mem v st.ctrs then Some (Mcvalue_ctr (v, []))
                      else None
  | _ -> None

and translate_cvalue st cv =
  match cv with
  | CValue_ivalue iv
    -> begin
        match tryn_make_constructor st iv with
	| Some cv' -> cv'
	| None -> Mcvalue_ivalue (translate_ivalue st iv)
       end
  | CValue_ctr (k, vs)
    -> if CtrSet.mem k st.ctrs then
	Mcvalue_ctr (k, List.map (translate_cvalue st) vs)
       else
	invalid_constructor k st.def_name
  | CValue_thunk cc
    -> st.def_name <- "thunk inside " ^ st.def_name;
       Mcvalue_thunk (translate_ccomp st cc)

and translate_hdr_cse st (ps, cc) =
  let ps' = List.map (refine_pat st) ps in
  let cc' = translate_ccomp st cc in (ps', cc')

and translate_clause st cse =
  match cse with
  | CComp_hdr_clause (ps, cc)
    -> Mccomp_clauses [translate_hdr_cse st (ps, cc)]
  | CComp_compose cs
    -> let f =
	 function  CComp_hdr_clause (ps, cc) -> translate_hdr_cse st (ps, cc)
	         |          _              -> invalid_clause_error st.def_name
       in Mccomp_clauses (List.map f cs)
  | _ -> invalid_clause_error st.def_name

let translate_hdr st def =
  let rpat = List.map (refine_pat st) def.vdef_args in
  let midcomp = translate_ccomp st def.vdef_comp in
  (rpat, midcomp)

(** Functions to construct mid-level handlers of a program from
    the declaration and clause fragments. *)

(* Environment for mapping tvars to rigid tvars. *)
module ENV = Map.Make(String)

(* Collect the names of the free type variables. *)
let rec free_tvars t =
  match t.styp_desc with
  | Styp_datatype (_, ts)
  | Styp_effin (_, ts)   -> List.flatten (map free_tvars ts)

  | Styp_thunk t         -> free_tvars t

  | Styp_comp (ts, t)
  | Styp_ret (ts, t)     -> (List.flatten (map free_tvars ts)) ++ free_tvars t

  | Styp_tvar v -> [v]

  | _ -> []

(* Return [true] if [t] has a unique set of type variables,
   [false] otherwise. *)
let uniq_tvars ts =
  let xs = List.flatten (map free_tvars ts) in
  let ys = List.sort_uniq String.compare xs in
  (List.length xs - List.length ys) == 0

(* Perform some desugaring of the type:

   * Append a singleton set containing the effect variable to each effect set
   * Convert builtins that are parsed as datatypes to their corresponding
     builtin types
   * Ensure uniqueness of type variables within a particular scope
   * Convert user provided type variables to rigid type variables. *)
let rec desugar_type' env t = 
  match t.styp_desc with
  | Styp_tvar v -> begin
                     try env, ENV.find v env with
		     | Not_found -> (* Generate fresh rigid tvar *)
		       let rtvar = TypExp.fresh_rigid_tvar v in
		       ENV.add v rtvar env, rtvar
                   end
  | Styp_datatype ("Int", [])    -> env, TypExp.int ()
  | Styp_datatype ("Float", [])  -> env, TypExp.float ()
  | Styp_datatype ("Bool", [])   -> env, TypExp.bool ()
  | Styp_datatype ("String", []) -> env, TypExp.str ()
  | Styp_datatype (d, ps)
    -> let (env, ps) = map_accum desugar_type' env ps in
       env, TypExp.datatype d ps
  | Styp_thunk c ->
    let (env, c) = desugar_type' env c in
    env, TypExp.sus_comp c
  | Styp_comp (ts, r)
    -> let (env, ts) = map_accum desugar_type' env ts in
       let (env, r) = desugar_type' env r in
       env, TypExp.comp ~args:ts r
  | Styp_ret (es, v)
    -> let (env, es) = map_accum desugar_type' env es in
       let (env, v) = desugar_type' env v in
       let evar = TypExp.effect_var_set in
       env, TypExp.returner v ~effs:(evar ++ es) ()
  | Styp_effin (ei, ps) ->
    let (env, ps) = map_accum desugar_type' env ps in
    env, TypExp.effin ei ~params:ps ()
  | _ -> env, t

let desugar_datatype dt =
  let desugar_ctr env ctr =
    let (env, ts) = map_accum desugar_type' env ctr.sctr_args in
    let (env, r) = desugar_type' env ctr.sctr_res in
    env, { ctr with sctr_args = ts; sctr_res = r } in
  let ps = dt.sdt_parameters in
  if uniq_tvars ps then
    let (env, ps) = map_accum desugar_type' ENV.empty ps in
    let (_, ctrs) = map_accum desugar_ctr env dt.sdt_constructors in
    { dt with sdt_parameters = ps; sdt_constructors = ctrs }
  else
    let d = dt.sdt_name in
    duplicate_tvar ("in parameter list of datatype " ^ d)

let desugar_effect_interface ei =
  let desugar_cmd env cmd =
    let (env, ts) = map_accum desugar_type' env cmd.scmd_args in
    let (env, r) = desugar_type' env cmd.scmd_res in
    env, { cmd with scmd_args = ts; scmd_res = r } in
  let ps = ei.sei_parameters in
  if uniq_tvars ps then
    let (env, ps) = map_accum desugar_type' ENV.empty ps in
    let (_, cmds) = map_accum desugar_cmd env ei.sei_commands in
    {ei with sei_parameters = ps; sei_commands = cmds }
  else
    let e = ei.sei_name in    
    duplicate_tvar ("in parameter list of effect interface " ^ e)

let desugar_hdr t = snd (desugar_type' ENV.empty t)

let make_hdr st (defs, hs) d =
  st.def_name <- d.svdecl_name;
  let name_eq def = def.vdef_name = d.svdecl_name in
  let (hdr_defs, defs) = List.partition name_eq defs in
  let hdr_clauses = List.map (translate_hdr st) hdr_defs in
  let h =
    {
      mhdr_name = d.svdecl_name;
      mhdr_type = desugar_hdr d.svdecl_type;
      mhdr_defs = hdr_clauses
    }
  in (defs, h :: hs)

let make_hdr_defs st decls defs =
  let acc = (defs, []) in
  let (_, hdrs) = List.fold_left (make_hdr st) acc decls in
  hdrs

(** Functions to compose the components of a mid-level tree into
    a complete list representing the program. *)

let get_hdrs hmap =
  let bindings = HandlerMap.bindings hmap in
  List.map (fun (k,hdr) -> Mtld_handler hdr) bindings

let merge dts eis hmap =
  List.map (fun dt -> Mtld_datatype dt) dts ++
    List.map (fun ei -> Mtld_effin ei) eis ++
    get_hdrs hmap

let disjoint_from_builtin_datatypes dt =
  let module M = Set.Make(String) in
  let env = M.add "Unit" M.empty in
  let name = dt.sdt_name in
  if M.mem name env then shadowing_builtin ("datatype " ^ name)
  else ()

let disjoint_from_builtin_interfaces ei =
  let module M = Set.Make(String) in
  let env = M.add "Random" (M.add "Console" M.empty) in
  let name = ei.sei_name in
  if M.mem name env then shadowing_builtin ("effect interface " ^ name)
  else ()

(* Builtin constructors *)
let builtin_ctrs = CtrSet.M.add "Unit" CtrSet.empty

(* Builtin commands *)
let builtin_cmds =
  CmdSet.M.add "random"
    (CmdSet.M.add "putStr"
       (CmdSet.M.add "putStrLn" (CmdSet.M.add "getStr" CmdSet.empty)))

(** Main translation function. *)

let translate prog =
  let (dts, eis, decls, defs) = partition prog in
  let _ = List.iter disjoint_from_builtin_datatypes dts in
  let _ = List.iter disjoint_from_builtin_interfaces eis in
  let dts = map desugar_datatype dts in
  let eis = map desugar_effect_interface eis in
  let ctrs = List.flatten (List.map (fun dt -> dt.sdt_constructors) dts) in
  let cmds = List.flatten (List.map (fun ei -> ei.sei_commands) eis) in
  let ctrs = List.fold_left add_ctr builtin_ctrs ctrs in
  let cmds = List.fold_left add_cmd builtin_cmds cmds in
  let state = { def_name = ""; ctrs; cmds } in
  let defs = make_hdr_defs state decls defs in
  let hmap = List.fold_left add_def HandlerMap.empty defs in
  if HandlerMap.mem "main" hmap then
    let mtree = merge dts eis hmap in
    (mtree, hmap, ctrs, cmds)
  else no_main ()
