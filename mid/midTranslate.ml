open MidTree
open ParseTree
open ParseTreeBuilder

type mid_error =
  | Merr_inv_clause of string
  | Merr_inv_ctr of string
  | Merr_no_main of string

exception Error of mid_error

let invalid_clause_error def =
  raise (Error
	   (Merr_inv_clause
	      ("Invalid pattern matching computation when parsing " ^ def)))

let invalid_constructor k def =
  raise (Error (Merr_inv_ctr
		  ("No such constructor " ^ k ^ " when parsing " ^ def)))

let no_main () = raise (Error (Merr_no_main "No main function defined."))

module type HMS = sig
  type t
  val empty : t
  (** Return an empty mapping. *)
  val lookup : string -> t -> handler_definition
  (** Raises a [Not_found] exception if not found. *)
  val mem : string -> t -> bool
  (** Return true if the map contains the specified string false
      otherwise. *)
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
  type t = handler_definition M.t
  let empty = M.empty
  let lookup h m = M.find h m
  let mem = M.mem
end

module CtrSet = struct
  module M = Set.Make(String)
  type t = M.t
  let empty = M.empty
  let mem = M.mem
end

module SigSet = struct
  module M = Set.Make(String)
  type t = M.t
  let empty = M.empty
  let mem = M.mem
end

type prog_state =
  {
    mutable def_name : string;
    cset : CtrSet.t;
    sset : SigSet.t
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

let add_sig set si = SigSet.M.add si.ssig_name set

let add_def map def = HandlerMap.M.add def.mhdr_name def map

(** Functions for refining the pattern matching of handlers. *)
let rec refine_vpat st vp =
  match vp with
  | Svpat_var v (** Probe constructor environment; return ctr if found. *)
    -> if CtrSet.mem v st.cset then Svpat_ctr (v, []) else vp   
  | Svpat_ctr (k, ps)
    -> let ps' = List.map (refine_vpat st) ps in Svpat_ctr (k, ps')

let refine_cpat st cp =
  match cp with
  | Scpat_request (s, ps, k)
    -> let ps' = List.map (refine_vpat st) ps in Scpat_request (s, ps', k)
  | _ -> cp

(** Using the constructor set, reconstruct the pattern lists
    to rectify any constructors incorrectly identified as variables. *)
let refine_pat st pat =
  match pat.spat_desc with
  | Spat_value vp -> Pattern.vpat(refine_vpat st vp)
  | Spat_comp cp -> Pattern.cpat(refine_cpat st cp)

(** Functions for translating the computation of a handler clause. *)

let rec translate_ccomp st cc =
  match cc with
  | CComp_cvalue cv -> Mccomp_cvalue (translate_cvalue st cv)
  | _ -> Mccomp_clause (translate_clause st cc)

and translate_icomp st ic =
  match ic with
  | IComp_force iv -> Micomp_force (translate_ivalue st iv)
  | IComp_app (iv, cs) -> let iv' = translate_ivalue st iv in
			  let cs' = List.map (translate_ccomp st) cs in
			  Micomp_app (iv', cs')

and translate_ivalue st iv =
  match iv with
  | IValue_ident v
    -> if SigSet.mem v st.sset then
	Mivalue_sig v
      else
	Mivalue_var v
  | IValue_int n -> Mivalue_int n
  | IValue_bool b -> Mivalue_bool b
  | IValue_icomp ic
    -> Mivalue_icomp (translate_icomp st ic)

and tryn_make_constructor st iv =
  match iv with
  | IValue_ident v -> if CtrSet.mem v st.cset then Some (Mcvalue_ctr (v, []))
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
    -> if CtrSet.mem k st.cset then
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
  | CComp_hdr_clause (ps, cc) -> Mcomp_clauses [translate_hdr_cse st (ps, cc)]
  | CComp_emp_clause -> Mcomp_emp_clause
  | CComp_compose cs
    -> let f =
	 function  CComp_hdr_clause (ps, cc) -> translate_hdr_cse st (ps, cc)
	         |          _              -> invalid_clause_error st.def_name
       in Mcomp_clauses (List.map f cs)
  | _ -> invalid_clause_error st.def_name

let translate_hdr st def =
  let rpat = List.map (refine_pat st) def.vdef_args in
  let midcomp = translate_ccomp st def.vdef_comp in
  (rpat, midcomp)

(** Functions to construct mid-level handlers of a program from
    the declaration and clause fragments. *)

let make_hdr st (defs, hs) d =
  st.def_name <- d.svdecl_name;
  let name_eq def = def.vdef_name = d.svdecl_name in
  let (hdr_defs, defs) = List.partition name_eq defs in
  let hdr_clauses = List.map (translate_hdr st) hdr_defs in
  let h =
    {
      mhdr_name = d.svdecl_name;
      mhdr_type = d.svdecl_type;
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
  let bindings = HandlerMap.M.bindings hmap in
  List.map (fun (k,hdr) -> Mtld_handler hdr) bindings

let merge dts eis hmap =
  List.map (fun dt -> Mtld_datatype dt) dts @
    List.map (fun ei -> Mtld_effin ei) eis @
    get_hdrs hmap

(** Main translation function. *)

let translate prog =
  let (dts, eis, decls, defs) = partition prog in
  let ctrs = List.flatten (List.map (fun dt -> dt.sdt_constructors) dts) in
  let sigs = List.flatten (List.map (fun ei -> ei.sei_signatures) eis) in
  let cset = List.fold_left add_ctr CtrSet.empty ctrs in
  let sset = List.fold_left add_sig SigSet.empty sigs in
  let state = { def_name = ""; cset; sset } in
  let defs = make_hdr_defs state decls defs in
  let hmap = List.fold_left add_def HandlerMap.empty defs in
  if HandlerMap.M.mem "main" hmap then
    let mtree = merge dts eis hmap in
    (mtree, hmap, cset, sset)
  else no_main ()
