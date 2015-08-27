(***********************************************************************
 * Useful utilities that I define here rather than import a library.
 *
 *
 * Created by Craig McLaughlin on 27/08/2015.
 ***********************************************************************
 *)

val (@) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(** Function composition *)

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
