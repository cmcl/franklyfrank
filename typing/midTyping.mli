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

type type_sig =
  TSAllValues
(* The top element of the lattice for value type signatures. *)
| TSInt of int
| TSFloat of float
| TSStr of string
| TSTrue
| TSFalse
(* Special cases for booleans since they have enumerable constructors. *)
| TSCmd of string
| TSCtr of string

(** The signature of a type; all its possible head constructors. *)

module type TSS = sig
  type t
  val empty : t
  (** Return an empty set. *)
  val mem : type_sig -> t -> bool
  (** Return true if the set contains the specified type signature false
      otherwise. *)
  val union : t -> t -> t
  (** Return the union of the provided sets. *)
  val add : type_sig -> t -> t
  (** Add an type signature to the set if it is not already present. *)
  val singleton : type_sig -> t
  (** Return a singleton set containing the specified element. *)
end

module TypeSigSet : TSS
(** Set containing type signatures. *)

val compute_signature : env -> ParseTree.src_type -> TypeSigSet.t
(** [compute_signature env t] compute the signature of the given type with
    respect to the given environment. *)

val type_prog : MidTree.prog -> ParseTree.src_type * env
(** Typecheck a mid-level tree and return the type and the constructed
    environment on success. On failure, a [TypeError] exception is
    raised. *)
