(***********************************************************************
 * Translate the untyped abstract syntax tree of the source language
 * into a mid-level language. It performs some processing on the AST to
 * distinguish handlers, constructors and commands from each other. It
 * also combines a handler's type declaration with its clauses.
 *
 *
 * Created by Craig McLaughlin on 21/07/2015.
 ***********************************************************************
 *)

open MidTree

type mid_error =
  | Merr_not_comp of string
  | Merr_inv_clause of string
  | Merr_inv_ctr of string
  | Merr_no_main of string
  | Merr_duplicate_tvar of string

exception Error of mid_error

module type HMS = sig
  include Map.S with type key := string
  type mt = handler_definition t
end

module type NS = sig
  type t
  val empty : t
  (** Return an empty set. *)
  val mem : string -> t -> bool
  (** Return true if the set contains the specified string false
      otherwise. *)
end

module HandlerMap : HMS
(** Map handler names to the corresponding definition. *)

module CtrSet : NS
(** Store for identifiers representing constructors. *)

module CmdSet : NS
(** Store for identifiers representing commands of effect interfaces. *)

val translate : ParseTree.prog ->
  MidTree.prog * HandlerMap.mt * CtrSet.t * CmdSet.t
(** Process the parse tree and return the mid-level tree and its associated
    mappings for global names (handlers), constructors and commands. *)
