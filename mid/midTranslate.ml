open MidTree
open ParseTree

module type HMS = sig
  type t
  val empty : t
  (** Return an empty mapping. *)
  val lookup : string -> t -> handler_definition
  (** Raises a [Not_found] exception if not found. *)
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
    cset : CtrSet.t;
    sset : SigSet.t
  }

let ex_datatype = function Sterm_datatype dt -> Some dt | _ -> None
let ex_effin = function Sterm_effin ei -> Some ei | _ -> None
let ex_decl = function Sterm_vdecl vd -> Some vd | _ -> None
let ex_def = function Sterm_vdefn vd -> Some vd | _ -> None
let rec extract xs =
  match xs with
    [] -> []
  | Some x :: xs -> x :: extract xs
  | _ :: xs -> extract xs

let partition prog =
  let ex f prog = List.map f prog |> extract in
  let dts   = ex ex_datatype prog in
  let eis   = ex ex_effin prog in
  let decls = ex ex_decl prog in
  let defs  = ex ex_def prog in
  (dts, eis, decls, defs)

let add_ctr set ctr = CtrSet.M.add ctr.sctr_name set

let add_sig set si = SigSet.M.add si.ssig_name set

let translate_ccomp st cc =
  match cc with
  | _ -> Mccomp_clause Mcomp_emp_clause

let translate_hdr st def =
  let midcomp = translate_ccomp st def.vdef_comp in
  (def.vdef_args, midcomp)

let add_decl st (defs, map) d =
  let name_eq def = def.vdef_name = d.svdecl_name in
  let (hdr_defs, defs) = List.partition name_eq defs in
  let hdr_clauses = List.map (translate_hdr st) hdr_defs in
  let hdr =
    {
      mhdr_name = d.svdecl_name;
      mhdr_type = d.svdecl_type;
      mhdr_defs = hdr_clauses
    }
  in (defs, HandlerMap.M.add d.svdecl_name hdr map)

let get_hdrs hmap =
  let bindings = HandlerMap.M.bindings hmap in
  List.map (fun (k,hdr) -> Mtld_handler hdr) bindings

let merge dts eis hmap =
  List.map (fun dt -> Mtld_datatype dt) dts @
    List.map (fun ei -> Mtld_effin ei) eis @
    get_hdrs hmap

let translate prog =
  let (dts, eis, decls, defs) = partition prog in
  let ctrs = List.flatten (List.map (fun dt -> dt.sdt_constructors) dts) in
  let sigs = List.flatten (List.map (fun ei -> ei.sei_signatures) eis) in
  let cset = List.fold_left add_ctr CtrSet.empty ctrs in
  let sset = List.fold_left add_sig SigSet.empty sigs in
  let state = { cset; sset } in
  let acc = (defs, HandlerMap.empty) in
  let (_, hmap) = List.fold_left (add_decl state) acc decls in
  let mtree = merge dts eis hmap in
  (mtree, hmap, cset, sset)
