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

let type_error msg = raise (TypeError msg)

let just_hdrs = function Mtld_handler hdr -> Some hdr | _ -> None

let rec type_prog prog =
  let tenv = ENV.add "Bool" TypeExp.bool
    (ENV.add "Int" TypExp.int ENV.empty) in
  let env = { tenv; fenv = ([], EVone); denv=ENV.empty } in
  let env = foldl type_tld env prog in
  try ENV.find "main" env.tenv with
  | Not_found -> type_error ("There must exist a unique main function")

and type_tld env d =
  match d with
  | Mtld_datatype dt -> type_datatype env dt
  | Mtld_effin    ei -> type_effect_interface env ei
  | Mtld_handler  h  -> type_hdr env h

and type_datatype env dt =
  let name = dt.sdt_name in
  let (env', ps) = map_accum inst env dt.sdt_parameters in
  let cs = map (inst_ctr env') dt.sdt_constructors in
  { env with denv = ENV.add name (ps, cs) env.denv }

and find_ctr env k d =
  let (_, cs) = try ENV.find d env.denv with
                | Not_found -> type_error ("Undefined datatype " ^ d)
  in
  let ctr = try List.find (fun ctr -> ctr.sctr_name = k) cs with
            | Not_found -> type_error ("No constructor named " ^ k ^
					  " for datatype " ^ d) in
  ctr

and type_effect_interface env ei = (* TODO: Make this do the correct thing *)
  let name = ei.sei_name in
  { env with tenv = ENV.add name (fresh_rigid_type_variable name) env.tenv }

and type_hdr env h =
  let name = h.mhdr_name in
  let t = h.mhdr_type in
  let env = {env with tenv = ENV.add name t env.tenv} in
  try type_clauses env t h with
  | TypeError msg -> type_error (msg ^ " in handler " ^ name)

(** Type variable instantiation procedures *)
and inst_ctr env ctr =
  let rec f env t =
    match t with
    | Styp_rtvar v
      -> begin
	  try ENV.find v env.tenv with
          | Not_found -> type_error ("Unrecognised type variable " ^ v ^
					" while checking constructor " ^
					ctr.sctr_name)
         end
    | _ -> snd (inst_with f env t)
  in
  { ctr with sctr_args = map (f env) ctr.sctr_args;
             sctr_res  = f env ctr.sctr_res; }

and inst env t =
  match t.styp_desc with
  | Styp_rtvar v
    -> try env, ENV.find v env.tenv with
       | Not_found ->
	 let point = Unionfind.fresh (Styp_ftvar v) in
	 let uvar = { styp_desc = Styp_ref point } in
	 let env = {env with tenv=ENV.add v uvar env.tenv} in
	 env, uvar
  | _ -> inst_with inst env t

and inst_with f env t =
  match t.styp_desc with
  | Styp_ret (es, v)
    -> let (env, es) = map_accum (inst_with f) env es in
       let (env, v) = inst_with f env v in
       env, TypExp.returner v ~effs:es ()
  | Styp_effin (e, ts)
    -> let (env, e) = inst_with f env e (*TODO: do the right thing here *) in
       let (env, ts) = map_accum (inst_with f) env ts in
       env, TypExp.effin e ~params:ts ()
  | Styp_rtvar v -> f t
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
  match cc with
  | Mccomp_cvalue  cv  -> type_cvalue env res cv
  | Mccomp_clauses cls -> type_clauses env res cls

and destruct_comp_type =
  function Styp_thunk (Styp_comp (args, res)) -> (args, res)
          |     _     -> type_error ("Incorrect handler type")

and type_clauses env t cls =
  let (ts, r) = destruct_comp_type t.styp_desc in
  foldl (type_clause env ts) r cls

and type_clause env ts r (ps, cc) =
  try
    let env = pat_matches env ts ps in
    type_ccomp env res cc
  with
  | TypeError s
    -> type_error (Printf.sprintf "%s when checking patterns..." s)

and pat_matches env ts ps =
  {env with tenv = foldl type_pattern env (zip ts ps)}

and pattern_error t p =
  let fmt = "pattern %s does not match expected type %s" in
  let msg = Printf.sprintf fmt (ShowPattern.show cp)
    (ShowSrcType.show t) in
  type_error msg

and type_pattern env (t, p) =
  match t.spat_desc, p with
  | _, Spat_any -> tenv
  | Spat_ret (es, v), Spat_thunk thk
    -> { env with tenv = ENV.add thk (TypExp.sus_comp t) env.tenv }
  | _, Spat_comp cp -> type_comp_pattern env (t, cp)
(* Value patterns only match value types *)
  | Styp_datatype _, Spat_value vp
  | Styp_thunk _, Spat_value vp
  | Styp_rtvar _, Spat_value vp
  | Styp_bool, Spat_value vp
  | Styp_int, Spat_value vp
    -> type_value_pattern env (t, vp)
  | _ , _ -> pattern_error t p

and type_comp_pattern env (t, cp) =
  match t.spat_desc, cp with
  | Styp_ret (es, v), Scpat_request (c, vs, r)
    -> let (ei, cmd) = try ENV.find c env.cenv with
                       | Not_found -> type_error ("undefined command " ^ c) in
       if is_handled ei es then
	 let ts = cmd.ssig_args in
	 if length ts = length vs then
	   let env = foldl type_value_pattern env (zip ts vs) in
	   (* TODO: effect set incorrect; need closed effect set not poly *)
	   let c = TypExp.comp [TypExp.returner cmd.ssig_res ()] t in
	   let sc = TypExp.sus_comp c in
	   { env with tenv = ENV.add r sc env.tenv }
	 else
	   let fmt = "command %s of %s expects %d arguments, %d given" in
	   let msg = Printf.sprintf fmt c ei (length ts) (length vs) in
	   type_error msg
       else
	 let fmt = "effect interface %s of command %s not handled by %s" in
	 let msg = Printf.sprintf fmt ei c (ShowSrcType.show t) in
	 type_error msg
  | _ , _ -> pattern_error t p

and is_handled ei es =
  let just_eis = function Styp_effin (ei, _) -> Some ei | _ -> None in
  List.mem ei (filter_map just_eis es)

and type_value_pattern env (t, vp) =
  match t.spat_desc, vp with
  | Styp_bool, Svpat_bool _
  | Styp_int, Svpat_int _
  | _, Svpat_any
    -> env
  | _, Svpat_var x -> { env with tenv = ENV.add x t env.tenv }
  | Styp_datatype (d, ps), Svpat_ctr (k, vs)
    -> let ctr = find_ctr env k d in
       let () = validate_ctr_use ctr d vs in
       let ts = ctr.sctr_args in
       foldl type_value_pattern env (zip ts vs)

and type_cvalue env res cv =
  match cv, res with
  | Mcvalue_ivalue iv, _
    -> let t = type_ivalue env iv in
       unify env res t
  | Mcvalue_ctr (k, vs), Styp_datatype (d, ts) -> type_ctr env (k, vs) (d, ts)
  | Mcvalue_thunk cc, Styp_thunk c -> type_ccomp env c cc (** TODO: Add
						     coverage checking *)
  | _ , _ -> type_error ("Cannot check " ^ ShowMidCValue.show cv ^
			    " against " ^ ShowSrcType.show res)

and validate_ctr_use ctr d vs =
  let k = ctr.sctr_name in
  let ts = ctr.sctr_args in
  if length ts != length vs then
    let fmt = "constructor %s of %s expects %d arguments, %d given" in
    let msg = Printf.sprintf fmt k d (length ts) (length vs) in
    type_error msg

and type_ctr env (k, vs) (d, ts) =
  let ctr = find_ctr env k d in
  let () = validate_ctr_use ctr d vs in
  let ts = ctr.sctr_args in
  (* The following map operation will blow up (raise a TypeError
     exception) if we cannot unify the argument types with the provided
     values. *)
  let _ = map (fun (t, v) -> type_cvalue env t v) (zip ts vs) in
  TypExp.datatype d ts (* Typechecked datatype *)

(** env |- iv infers (type_ivalue env iv) *)
and type_ivalue env iv =
  match iv with
  | Mivalue_var v -> ENV.find v env.tenv
  | Mivalue_sig s -> type_sig env s
  | Mivalue_int _ -> TypExp.int
  | Mivalue_bool _ -> TypExp.bool
  | Mivalue_icomp ic -> type_icomp env ic

and type_var env res v =
  try (* TODO: Implement equality *)
    if ENV.find v env = res then res
    else raise (TypeError ("var type mismatch"))
  with
  | Not_found -> raise (TypeError ("No such var " ^ v))

and type_sig env res v = (* TODO: Something sensible here *)

and type_icomp env ic =
  match ic with
  | Micomp_app (iv, cs) -> (* TODO: An actual thing *)

and unify env x y = (* TODO: perform unification *)
