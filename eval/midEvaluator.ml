(***********************************************************************
 * Evaluate the mid-level language.
 *
 *
 * Created by Craig McLaughlin on 21/07/2015.
 ***********************************************************************
 *)

open Monad

module type COMP = sig
  include MONAD

  type comp = value t
  and  value =
    | VInt of int
    | VCon of string * value list
    | VMultiHandler of (comp list -> comp)

  val (>=>) : (value -> 'a t) -> ('a -> 'b t) -> value -> 'b t
  val sequence : ('a t) list -> ('a list) t
  val command : string -> value list -> comp
end

module Comp : COMP = struct
  type 'a t =
    | Command of string * value list * (value -> 'a t)
    | Return of 'a

  and comp = value t

  and value =
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
end

type env = string -> Comp.value
type bindings_pre_env = string -> (env -> Comp.comp list -> Comp.comp)

let rec tie : bindings_pre_env -> env =
  fun pre_env x -> Comp.VMultiHandler (pre_env x (tie pre_env))

let eval prog = ()
