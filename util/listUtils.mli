(**
 * Some list helper functions which are supplied by other libraries but for
 * which I'd rather not create an unnecessary dependency.
 *
 * Created by Craig McLaughlin on 29/07/2015.
***********************************************************************)

val (++) : 'a list -> 'a list -> 'a list

val repeat : 'a -> int -> 'a list
(** [repeat x n] returns a list of length [n] all elements of which are
    initialised to [x]. Return the empty list for [n <= 0]. *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
 (** [filter_map f as]: Return the elements of [as] for which [f] returned
     a [Some b] for some [b]. In other words, discard the [None]s. *)

val map : ('a -> 'b) -> 'a list -> 'b list

val zip : 'a list -> 'b list -> ('a * 'b) list

val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val length : 'a list -> int

val map_accum : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
  (** Traverse the list accumulating a result as the first element of
      a pair and the resulting list as the second element. *)

val swap : 'a list -> int -> int -> 'a list
(** [swap xs i j] swap element at position [i] with the element at position
    [j] and return the resulting list. *)

val transpose : 'a list list -> 'a list list
  (** Take a NxM matrix and return an MxN matrix. Raises a Failure exception
      if any of the inner lists are empty. *)
