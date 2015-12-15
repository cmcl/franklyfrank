(***********************************************************************
 * Definition of the typing of the mid-level tree.
 *
 *
 * Created by Craig McLaughlin on 7/08/2015.
 ***********************************************************************
 *)

exception TypeError of string

type env
(** The typing environment constructed during typechecking. *)

type type_sig
(** The signature of a type; all its possible head constructors. *)

module type TSS = sig
  type t
  val empty : t
  (** Return an empty set. *)
  val mem : type_sig -> t -> bool
  (** Return true if the set contains the specified type signature false
      otherwise. *)
end

module TypeSigSet : TSS
(** Set containing type signatures. *)

val compute_signature : env -> ParseTree.src_type -> TypeSigSet.t

val type_prog : MidTree.prog -> ParseTree.src_type * env
(** Typecheck a mid-level tree and return the type and the constructed
    environment on success. On failure, a [TypeError] exception is
    raised. *)
