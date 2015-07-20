(***********************************************************************
 * Collection of helper functions to construct components of the
 * parse tree (untyped AST). Inspired by the frontend design of the
 * OCaml compiler.
 * 
 *
 * Created by Craig McLaughlin on 30/06/2015.
 ***********************************************************************
 *)

open ParseTree

(** Terms *)
module Term :
  sig
    val datatype : datatype_declaration -> term
    val effect_in : effect_interface -> term
    val value_decl : value_declaration -> term
    val value_defn : value_definition -> term
  end

(** Checkable computations *)
module CComputation :
  sig
    val cvalue : checkable_value -> checkable_computation
    val clause : pattern list -> checkable_computation ->
      checkable_computation
    val compose : checkable_computation list -> checkable_computation
  end

(** Checkable values *)
module CValue :
  sig
    val ivalue : inferable_value -> checkable_value
    val sus_comp : checkable_computation -> checkable_value
    val ctr : string -> checkable_value list -> checkable_value
  end

(** Inferable values *)
module IValue :
  sig
    val ident : string -> inferable_value
    val icomp : inferable_computation -> inferable_value
  end

(** Inferable computation *)
module IComp :
  sig
    val forced_thunk : inferable_value -> inferable_computation
    val app : inferable_computation -> checkable_computation ->
      inferable_computation
  end

(** Datatype declarations *)
module Datatype :
  sig
    val mk : string -> ?params:src_type list ->
      ?ctrs:constructor_declaration list -> unit -> datatype_declaration

    val constr_decl : string -> ?args:src_type list -> src_type ->
      constructor_declaration
  end

(** Effect interface *)
module EffInterface :
  sig
    val mk : string -> ?params:src_type list -> ?sigs:src_type list ->
      unit -> effect_interface
  end

(** Value declarations *)
module ValueDecl :
  sig
    val mk : string -> src_type -> value_declaration
  end

(** Type expressions *)
module TypExp :
  sig
    val mk : src_type_desc -> src_type

    val var : string -> src_type
    val ctr : string -> src_type list -> src_type
    val effect_sig : string -> src_type -> src_type
    val effin : string -> ?params:src_type list -> unit -> src_type
    val sus_comp : src_type -> src_type
    val comp : src_type list -> src_type -> src_type
    val returner : src_type -> ?effs:src_type list -> unit -> src_type
  end

(** Value definitions *)
module ValueDefn :
  sig
    val mk : string -> ?pats:pattern list -> checkable_computation ->
      value_definition
  end

(** Patterns *)
module Pattern :
  sig
    val mk : pattern_desc -> pattern

    val vpat : value_pattern -> pattern
    val cpat : computation_pattern -> pattern

    val var : string -> value_pattern
    val ctr : string -> ?pats:value_pattern list -> unit ->
      value_pattern
    val request : string -> ?pats:value_pattern list -> string ->
      computation_pattern
    val thunk : string -> computation_pattern
  end
