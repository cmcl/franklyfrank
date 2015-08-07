open MidTree
open ParseTree
open ListUtils

exception TypeError of string

module ENV = Map.Make(String)

let just_hdrs = function Mtld_handler hdr -> Some hdr | _ -> None

let rec type_prog prog =
  let env = ENV.add "Bool" TypeExp.bool
    (ENV.add "Int" TypExp.int ENV.empty) in
  let ts = List.map (type_tld env) prog in
  let hdrs = filter_map just_hdrs prog in
  match filter (fun (ts,h) -> h.mhdr_name = "main") (zip ts hdrs) with
  | [(t,_)] -> t
  | _
    -> raise (TypeError ("There's only one main function... walking in a..."))

and type_tld d =
  match d with
  | Mtld_datatype dt -> type_datatype env dt
  | Mtld_effin    ei -> type_effect_interface env ei
  | Mtld_handler  h  -> type_hdr env h

and type_datatype env dt = Styp_var(dt.sdt_name)

and type_effect_interface env ei = Styp_var(ei.sei_name)

and type_hdr env h =
  let _ = type_clauses env h.mhdr_type h.mhdr_defs in
  h.mhdr_type

and type_pattern (arg, p) =
  (** TODO: Consult some enviornment to determine the type of p
      and compare to arg. *)
  raise (TypeError "Expecting typeof(arg) got typeof(p)")

(** env |- res checks cc *)
and type_ccomp env res cc =
  match cc with
  | Mccomp_cvalue  cv  -> type_cvalue env res cv
  | Mccomp_clauses cls -> type_clauses env res cls

and type_clauses env t cls =
  let (args,res) = destruct_arrow_type t in foldl (type_clause args) res cls

and type_clause args res (ps, cc) =
  try
    let env  = pat_matches args ps in
    type_ccomp env res cc
  with
  | TypeError s -> Debug.print "%s...\n" s; exit(-1)

and pat_matches args ps =
  foldl type_pattern ENV.empty (zip args ps)

and type_pattern env (t, p) = raise (TypeError ("Pattern match fail"))

and destruct_arrow_type t =
  let rec f t rargs =
    match t with
    | Styp_arrow a b -> f b (a :: rargs)
    | _ -> (List.rev rargs, t)
  in f t []

(** env |- res checks cv *)
and type_cvalue env res cv =
  match cv with
  | Mcvalue_ivalue iv   -> type_ivalue env res iv (** TODO: Add unification *)
  | Mcvalue_ctr (k, vs) -> type_ctr env res k vs
  | Mcvalue_thunk cc    -> type_ccomp env res cc (** TODO: Add
						     coverage checking *)
(** env |- res infers iv *)
and type_ivalue env res iv =
  match iv with
  | Mivalue_var v -> type_var env res v
  | Mivalue_sig s -> type_sig env res s
  | Mivalue_int i -> TypExp.int


and type_var env res v =
  try
    if ENV.find v env = res then res
    else raise (TypeError ("var type mismatch"))
  with
  | Not_found -> raise (TypeError ("No such var " ^ v))

  
