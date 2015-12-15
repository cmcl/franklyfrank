(* Pattern matching compilation module.

*)

type 'a vector = 'a list
type 'a matrix = 'a vector vector
type action = MidTree.mid_ccomputation
type pattern = MidTree.pattern
type value = MidTree.mid_cvalue
(** Helpful synonyms. *)

type clause = pattern vector * action
type pmatrix = pattern matrix
type cmatrix = clause vector
(** Shorthands. *)

type dtree =
  Fail
| Leaf of action
| Swap of int * dtree (* Subterm to be inspected w.r.t the decision tree. *)
| Switch of case list
(** Representation for decision trees the target of pattern matching
    compilation. *)

and case =
  CseDefault of dtree
| CseCtr of string * dtree
(** Cases which occur at multi-way test nodes within a decision tree. *)

(* Pattern and value operations. *)

val is_inst : pattern -> value -> bool
val is_inst_vec : pattern vector -> value vector -> bool
(** [is_inst p v] returns [true] ([false] otherwise) if [v] is an instance
    of pattern [p]. Defined as (NB: infix notation "<=" for is_instance):
    * _ <= v
    * c(p1,...,pN) <= c(v1,...,vN) iff (p1,...,pN) <= (v1,...,vN)
    * (p1,...,pN) <= (v1,...,vN) iff for all i, pi <= vi
    The last case is on sequences of patterns (a pattern vector).
*)

val not_inst : pattern -> value -> bool
(** [not_inst p v] returns [true] ([false] otherwise) if [v] is not an
    instance of pattern [p]. For the time being, this is simply the negation
    of [is_inst] but I am reliably informed this will not remain so. *)

val string_of_pattern : pattern -> string
val string_of_patterns : pattern vector -> string
(** [string_of_pattern p] return a string representation of pattern [p]. *)

(* Matrix operations *)

val to_columns : cmatrix -> pmatrix * action vector
(** [to_columns m] returns the matrix as a pattern matrix and a column of
    actions. *)

val of_columns : pmatrix -> action vector -> cmatrix
(** [of_columns ps rs] returns a matrix computed from the transpose of the
    pattern matrix and the column of actions. *)

val prmatrix : cmatrix -> unit
(** [prmatrix m] print the matrix m to standard output. *)

(* Matrix decomposition operations. *)

val specialise : string -> int -> cmatrix -> cmatrix
(** [specialise c n m] simplify [m] by assuming that the first value admits
    [c] as a head constructor (with arity [n]) and return the resulting
    simplified clause matrix. *)

val default : cmatrix -> cmatrix
(** [default m] returns the "default" matrix computed from [m] which retains
    the rows of [m] whose first pattern admits as instances all constructors
    that are not present in the first column of [m]. *)

(* Matching, evaluation and compilation operations. *)

val matches : pmatrix -> value -> int option
(** [matches p v] returns the row [Some j] of [p] which filters [v] i.e. [v]
    matches row [Some j]. [None] is returned if [v] does not match any row in
    [p].*)

val eval_dtree : value list -> dtree -> action
(** [eval vs t] evaluates the decision tree [t] w.r.t the stack of values [vs]
    returning an action. The stack is assumed to initially hold the subject
    value. *)

val compile : cmatrix -> dtree
(** [compile m] returns the decision tree corresponding to the clause
    matrix [m]. *)
