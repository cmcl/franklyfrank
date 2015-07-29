(***********************************************************************
 * Evaluator defined in terms of a denotational semantics for the
 * mid-level language.
 *
 *
 * Created by Craig McLaughlin on 21/07/2015.
 ***********************************************************************
 *)

open Monad

(*i | k vs
   | command
   | top-level-handler (name)
   | built-in-handler (name)
   | local-handler (handler-def, locals)
   | continuation *)

module type COMP = sig
  include MONAD

  type comp = value t
  and value =
    | VBool of bool
    | VInt of int
    | VCon of string * value list
    | VMultiHandler of (comp list -> comp)

  val (>=>) : (value -> 'a t) -> ('a -> 'b t) -> value -> 'b t
  (** Kleisli composition of computations *)

  val sequence : ('a t) list -> ('a list) t
  val command : string -> value list -> comp
end

module Comp : COMP
(** Module representing monadic computation trees. *)

type env = string -> Comp.value
type bindings_pre_env = string -> (env -> Comp.comp list -> Comp.comp)

val tie : bindings_pre_env -> env

(* increment function *)
(* command "get" [] >>= (fun (Int x) -> *)
(*   command "put" [return (Int (x+1))]) *)

(* Command ("put", [v], r) is matched by [put x -> k] and [?c -> k] and [t] *)
(* Return v is matched by x *)
(* Return (suc v) is matched by suc x and x *)

val eval : MidTree.prog -> Comp.comp
