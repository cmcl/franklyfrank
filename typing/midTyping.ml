open MidTree
open ParseTree
open ListUtils

exception TypeError of string

module ENV = Map.Make(String)

type effect_var = EVempty | EVone

type effect_env = src_type list * effect_var

type datatype_env = ENV.t

type env =
  {
    tenv : ENV.t; (** Type environment mapping variables to their types. *)
    fenv : effect_env; (** Effect environment *)
    denv : datatype_env; (** Map datatypes to a pair consisting of parameters
			     and a list of constructors. *)
  }

let just_hdrs = function Mtld_handler hdr -> Some hdr | _ -> None

let rec type_prog prog =
  let tenv = ENV.add "Bool" TypeExp.bool
    (ENV.add "Int" TypExp.int ENV.empty) in
  let env = { tenv; fenv = ([], EVone); denv=ENV.empty } in
  let env = foldl type_tld env prog in
  let env = type_hdrs env prog in
  try ENV.find "main" env.tenv with
  | Not_found -> raise (TypeError ("There must exist a unique main function"))

and type_tld env d =
  match d with
  | Mtld_datatype dt -> type_datatype env dt
  | Mtld_effin    ei -> type_effect_interface env ei
  | Mtld_handler  h  -> add_hdr_type env h

and type_datatype env dt = (* TODO: Make this do the correct thing *)
  let name = dt.sdt_name in
  { env with tenv = ENV.add name (fresh_rigid_type_variable name) env.tenv }

and type_effect_interface env ei = (* TODO: Make this do the correct thing *)
  let name = ei.sei_name in
  { env with tenv = ENV.add name (fresh_rigid_type_variable name) env.tenv }

and add_hdr_type env h =
  let (_, t) = inst env h.mhdr_type in
  {env with tenv = ENV.add h.mhdr_name t env.tenv }

and inst' env t =
  match t with
  | Styp_ret (es, v)
    -> let (env, es) = map_accum inst' env es in
       let (env, v) = inst' env v in
       env, Styp_ret (es, v)
  | Styp_effin (e, ts)
    -> let (env, e) = inst' env e in
       let (env, ts) = map_accum inst' env ts in
       env, Styp_effin (e, ts)
  | Styp_rtvar v
    -> try env, ENV.find v env.tenv with
       | Not_found ->
	 let point = Unionfind.fresh (Styp_ftvar v) in
	 let uvar = Styp_ref point in
	 let env = {env with tenv=ENV.add v uvar env.tenv} in
	 env, uvar
  | Styp_ctr (k, vs)
    -> let (env, vs) = map_accum inst' env vs in
       env, Styp_ctr (k, vs)
  | Styp_thunk c -> let (env, c) = inst' env c in env, Styp_thunk c
  | Styp_comp (ts, r)
    -> let (env, ts) = map_accum inst' env ts in
       let (env, r) = inst' env r in
       Styp_comp (ts, r)
  | Styp_ftvar _ (* Will never be outside a ref *)
  | Styp_ref _
  | Styp_bool
  | Styp_int -> t

and inst env t = (** Instantiate the type t *)
  match t with
  | Styp_thunk (Styp_comp (args, res))
    -> let (env, args) = map_accum inst' env args in
       let (env, res) = inst' env res in
       env, Styp_thunk (Styp_comp (args, res))
  | _ -> env, t (** Monotype *)

and strvar v n = v ^ (string_of_int n)

and type_hdrs env prog = foldl type_hdr env (filter_map just_hdrs prog)

and type_hdr env h =
  let name = h.mhdr_name in 
  let t = ENV.find name env.tenv in
  try type_clauses env t h with
  | TypeError msg -> raise (TypeError (msg ^ " in handler " ^ name))

(** env |- res checks cc *)
and type_ccomp env res cc =
  match cc with
  | Mccomp_cvalue  cv  -> type_cvalue env res cv
  | Mccomp_clauses cls -> type_clauses env res cls

and destruct_comp_type =
  function Styp_thunk (Styp_comp (args, res)) -> (args, res)
          |     _     -> raise (TypeError ("Incorrect handler type"))

and type_clauses env t cls =
  let (ts, r) = destruct_comp_type t in
  foldl (type_clause env ts) r cls

and type_clause env ts r (ps, cc) =
  try
    let env  = pat_matches env ts ps in
    type_ccomp env res cc
  with
  | TypeError s -> Debug.print "%s when checking patterns..." s; exit(-1)

and pat_matches env args ps =
  {env with tenv=foldl type_pattern env.tenv (zip args ps)}

and type_pattern env (t, p) =
  (** TODO: Consult some enviornment to determine the type of p
      and compare to arg. *)
  raise (TypeError "Expecting typeof(arg) got typeof(p)")

(** env |- res checks cv *)
and type_cvalue env res cv =
  match cv with
  | Mcvalue_ivalue iv
    -> let t = type_ivalue env iv in
       unify res t
  | Mcvalue_ctr (k, vs), Styp_ctr (k', ts)
    -> if k = k' then type_ctr env ts k vs
       else raise (TypeError ("Expecting " ^ k' ^ " got " ^ k))
  | Mcvalue_thunk cc    -> type_ccomp env res cc (** TODO: Add
						     coverage checking *)
and type_ctr env res k vs =
  let env' = foldl (type_cvalue env) ENV.empty vs in

(** env |- iv infers (type_ivalue env iv) *)
and type_ivalue env iv =
  match iv with
  | Mivalue_var v -> ENV.find v env.tenv
  | Mivalue_sig s -> type_sig env s
  | Mivalue_int _ -> TypExp.int
  | Mivalue_bool _ -> TypExp.bool
  | Mivalue_icomp ic -> type_icomp env ic

and type_icomp env ic =
  match ic with
  | Micomp_app (iv, cs) ->

and type_var env res v =
  try
    if ENV.find v env = res then res
    else raise (TypeError ("var type mismatch"))
  with
  | Not_found -> raise (TypeError ("No such var " ^ v))

  
