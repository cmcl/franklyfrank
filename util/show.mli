(***********************************************************************
 * Show Typeclass as a module.
 *
 * Created by Craig McLaughlin on 28/07/2015.
 ***********************************************************************
 *)

module type SHOW = sig
  type t
  val show : t -> string
end

module ShowList (X : SHOW) : SHOW with type t = X.t list
(** Default show operation over lists. *)
