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
  | Merr_inv_clause of string
  | Merr_inv_ctr of string
  | Merr_no_main of string

exception Error of mid_error

module type HMS = sig
  type t
  val empty : t
  (** Return an empty mapping. *)
  val lookup : string -> t -> handler_definition
  (** Raises a [Not_found] exception if not found. *)
  val mem : string -> t -> bool
  (** Return true if the map contains the specified string false
      otherwise. *)
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

module SigSet : NS
(** Store for identifiers representing signatures of effect interfaces. *)

val translate : ParseTree.prog ->
  MidTree.prog * HandlerMap.t * CtrSet.t * SigSet.t
(** Process the parse tree and return the mid-level tree and its associated
    mappings for global names (handlers), constructors and commands. *)
