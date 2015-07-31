(***********************************************************************
 * Debugging facility for displaying intermediate results and internal
 * program state.
 *
 *
 * Created by Craig McLaughlin on 30/07/2015.
 ***********************************************************************
 *)

val debug_flag : bool -> unit
(** [debug_flag true] enables debugging. [debug_flag false] disables
    debugging. Initially, debugging is disabled. *)

val print : ('a, out_channel, unit) format -> 'a
(** Wrapper over the functionality of [Printf.printf]. *)
