(***********************************************************************
 * Definition of the typing of the mid-level tree.
 *
 *
 * Created by Craig McLaughlin on 7/08/2015.
 ***********************************************************************
 *)

exception TypeError of string

val type_prog : MidTree.prog -> ParseTree.src_type
  (** Typecheck a mid-level tree and return the type on success. On failure,
      a [TypeError] exception is raised. *)
