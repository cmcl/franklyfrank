(*pp deriving *)
(***********************************************************************
 * Definition of the typing of the mid-level tree.
 *
 *
 * Created by Craig McLaughlin on 7/08/2015.
 ***********************************************************************
 *)

exception TypeError of string

type src_type = ParseTree.src_type

type env
(** The typing environment constructed during typechecking. *)

type type_sig =
  TSAmbientCmds (* Greatest lower bound for TSCmd signatures w.r.t ambient
		   effects. *)
| TSAllValues of string option
(* The top element of the lattice for value type signatures. *)
| TSBool of bool
| TSFloat of float
| TSInt of int
| TSStr of string
| TSCmd of string * int (* constructor name and arity. *)
| TSCtr of string * int (* command name and arity *)
    deriving (Show)
(** The signature of a type; all its possible head constructors. *)

module type TSS = sig
  type t
  val empty : t
  (** Return an empty set. *)
  val mem : type_sig -> t -> bool
  (** Return true if the set contains the specified type signature false
      otherwise. *)
  val union : t -> t -> t
  (** [union s1 s2] returns the union of [s1] and [s2] with the elements in
      [s1] preceding the elements of [s2] in the "insertion" order. *)
  val add : type_sig -> t -> t
  (** Add an type signature to the set if it is not already present. *)
  val singleton : type_sig -> t
  (** Return a singleton set containing the specified element. *)
  val elements : t -> type_sig list
  (** Return the list of elements of the given set. The ordering of the
      elements in the list respects their insertion order. *)
  val diff : t -> t -> t
  (** [diff t u] returns the set difference t/u w.r.t to the partial order
      defined by the type_sig datatype. *)
  val is_ambient : t -> bool
  (** [is_ambient t] is [true] iff [t] is a singleton which contains the
      [TSAmbientCmds] type signature and [false] otherwise. *)
  val all_cmds : t -> bool
  (** [all_cmds t] is [true] iff [t] contains only signatures of the form
      TSCmd or TSAmbientCmds and [false] otherwise. *)
  val is_empty : t -> bool
  (** [is_empty t] is [true] iff [t] is empty and [false] otherwise. *)
end

module TypeSigSet : TSS
(** Set containing type signatures. *)

val compute_signature : env -> src_type -> TypeSigSet.t
(** [compute_signature env t] compute the signature of the given type with
    respect to the given environment. *)

val compute_arg_types : env -> src_type -> type_sig -> src_type list
(** [compute_arg_types env t tsg] computes the types that are arguments of the
    type signature [tsg] w.r.t the type [t], the concrete type of the type
    signature. [t] may provide information to the procedure such as the type
    at which to instantiate any parameters. *)

val env_lookup : string -> env -> src_type
(** [env_lookup x env] lookup the string [x] in the typing environment [env]
    returning the corresponding type. *)

val type_prog : MidTree.prog -> src_type * env
(** Typecheck a mid-level tree and return the type and the constructed
    environment on success. On failure, a [TypeError] exception is
    raised. *)

val type_error : string -> 'a
(** [type_error s] raise a TypeError exception with the provided string. *)
