(***********************************************************************
 * Evaluator defined in terms of a denotational semantics for the
 * mid-level language.
 *
 *
 * Created by Craig McLaughlin on 21/07/2015.
 ***********************************************************************
 *)

open Monad
open ParseTree

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
  val show : comp -> string
  val vshow : value -> string
  val pat_matches : comp list -> pattern list -> bool
end

module Comp : COMP
(** Module representing monadic computation trees. *)

(* increment function *)
(* command "get" [] >>= (fun (Int x) -> *)
(*   command "put" [return (Int (x+1))]) *)

val eval : MidTranslate.HandlerMap.mt -> MidTree.prog -> Comp.comp
