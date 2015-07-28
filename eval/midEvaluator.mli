(***********************************************************************
 * Evaluator for the mid-level language.
 *
 *
 * Created by Craig McLaughlin on 21/07/2015.
 ***********************************************************************
 *)

open Monad

(* globals : handler_def NameMap
   constructors : NameSet
   commands : NameSet
 *)

(*i | k vs
   | command
   | top-level-handler (name)
   | built-in-handler (name)
   | local-handler (handler-def, locals)
   | continuation *)


module type COMP = sig
  include MONAD
  type comp = value t
  val (>>>) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
  val sequence : ('a t) list -> ('a list) t
  val command : string -> value list -> comp
end

module Comp : COMP =
struct
  type 'a t =
  | Command of string * value list * (value -> 'a t)
  | Return of 'a
  and comp = value t
  and value =
  | Int of int
  | Con of string * value list
  | MultiHandler of comp list -> comp

  let lift : (value -> comp) -> value = fun f ->
    MultiHandler (fun [m] -> m >>= f)

  let return v = Return v
  let rec (>>=) m k =
    match m with
    | Command (c, vs, r) ->
      Command (c, vs, r >>> k)
    | Return v ->
      k v
  and (>>>) f g x = f x >>= g

  let rec sequence = function
    | []      -> return []
    | m :: ms -> m >>= (fun x -> sequence ms >>= (fun xs -> return (x :: xs)))

  let command c vs = Command (c, vs, return)
end


type env = string -> value
type bindings_pre_env = string -> (env -> comp list -> comp)

let rec tie : bindings_pre_env -> env =
  fun pre_env x -> MultiHandler (pre_env x (tie pre_env))

(* increment function *)
(* command "get" [] >>= (fun (Int x) -> *)
(*   command "put" [return (Int (x+1))]) *)



type EValue =
   EvalInt of int
 | EvalVar of MidTree.var
 | EvalCtr of string * EValue list



(* Command ("put", [v], r) is matched by [put x -> k] and [?c -> k] and [t] *)
(* Return v is matched by x *)
(* Return (suc v) is matched by suc x and x *)


val eval : MidTree.prog -> unit
