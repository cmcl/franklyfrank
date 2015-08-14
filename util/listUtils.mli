(**
 * Some list helper functions which are supplied by other libraries but for
 * which I'd rather not create an unnecessary dependency.
 *
 * Created by Craig McLaughlin on 29/07/2015.
***********************************************************************)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
 (** [filter_map f as]: Return the elements of [as] for which [f] returned
     a [Some b] for some [b]. In other words, discard the [None]s. *)

val map : ('a -> 'b) -> 'a list -> 'b list

val zip : 'a list -> 'b list -> ('a * 'b) list

val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val map_accum : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
  (** Traverse the list accumulating a result as the first element of
      a pair and the resulting list as the second element. *)
