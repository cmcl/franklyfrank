(***********************************************************************
 * Evaluate the mid-level language.
 *
 *
 * Created by Craig McLaughlin on 21/07/2015.
 ***********************************************************************
 *)

open MidTranslate
open MidTree
open Monad
open ParseTree

module type COMP = sig
  include MONAD

  type comp = value t
  and value =
    | VBool of bool
    | VInt of int
    | VCon of string * value list
    | VMultiHandler of (comp list -> comp)

  val (>=>) : (value -> 'a t) -> ('a -> 'b t) -> value -> 'b t
  val sequence : ('a t) list -> ('a list) t
  val command : string -> value list -> comp
  val show : comp -> string
end

module Comp : COMP = struct
  type 'a t =
    | Command of string * value list * (value -> 'a t)
    | Return of 'a

  and comp = value t

  and value =
    | VBool of bool
    | VInt of int
    | VCon of string * value list
    | VMultiHandler of (comp list -> comp)

  (** Monadic operations *)
  let return v = Return v

  let rec (>=>) f g x = f x >>= g
  and (>>=) m k =
    match m with
    | Command (c, vs, r) -> Command (c, vs, r >=> k)
    | Return v           -> k v

  let lift : (value -> comp) -> value = fun f ->
    VMultiHandler (fun [m] -> m >>= f)

  let rec sequence = function
    | []      -> return []
    | m :: ms -> m >>= (fun x -> sequence ms >>= (fun xs -> return (x :: xs)))

  let command c vs = Command (c, vs, return)

  let rec vshow v =
    match v with
    | VBool b -> string_of_bool b
    | VInt n -> string_of_int n
    | VCon (k, vs) -> "(" ^ k ^ (string_of_args " " vshow vs) ^ ")"
    | VMultiHandler _ -> "MULTIHANDLER"

  let show c =
    match c with
    | Command (c, vs, _)
      -> "Command (" ^ c ^ (string_of_args " " vshow vs) ^ ")"
    | Return v
      -> "Return (" ^ vshow v ^ ")"

end

open Comp

module ENV = Map.Make(String)

let just_hdrs = function Mtld_handler hdr -> Some hdr | _ -> None

let rec eval prog =
  let hdrs = ListUtils.filter_map just_hdrs prog in
  let pre_env = List.fold_right construct_env_entry hdrs ENV.empty in
  let rec env' =
    lazy (ENV.map (fun f ->
      return (VMultiHandler (fun cs -> f (Lazy.force env') cs))) pre_env) in
  let env = Lazy.force env' in
  let main = ENV.find "main" env in main

and construct_env_entry hdr env =
  ENV.add hdr.mhdr_name (fun env cs -> eval_tlhdrs env hdr.mhdr_defs cs) env

and eval_tlhdrs env cls cs =
  List.fold_right (eval_clause env cs) cls pat_match_fail

and eval_clause env cs (ps, e) acc =
  if pat_matches cs ps then eval_ccomp env e else acc

and pat_matches cs ps = false

and pat_match_fail = command "PatternMatchFail" []

and unhandled_comp = command "UnhandledComputation" []

and eval_ccomp env cc =
  match cc with
  | Mccomp_cvalue cv -> eval_cvalue env cv
  | Mccomp_clause cl -> unhandled_comp

and eval_cvalue env cv =
  match cv with
  | Mcvalue_ivalue iv -> eval_ivalue env iv
  | Mcvalue_ctr (k, vs) -> unhandled_comp
  | Mcvalue_thunk cc -> unhandled_comp

and eval_ivalue env iv =
  match iv with
  | Mivalue_var v -> if ENV.mem v env then eval_hdr env v
                     else eval_var env v
  | Mivalue_icomp ic -> eval_icomp env ic
  | _ -> unhandled_comp

and eval_icomp env ic =
  match ic with
  | Micomp_force iv -> eval_ivalue env iv
  | Micomp_app (iv, cs) -> unhandled_comp

and eval_var env x = ENV.find x env

and eval_hdr env f = unhandled_comp
