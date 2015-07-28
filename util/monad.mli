(***********************************************************************
 * Monadic module types and some basic functions.
 * 
 *
 * Created by Craig McLaughlin on 27/07/2015.
 ***********************************************************************
 *)


module type MONAD :
sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end
