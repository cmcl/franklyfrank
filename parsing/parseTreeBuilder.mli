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

(** Expressions *)
module Expressions :
  sig
    val var : string -> checkable_computation
    val suspended_comp : checkable_computation -> checkable_computation
    val call : checkable_computation -> checkable_computation ->
      checkable_computation
  end

(** Datatype declarations *)
module Datatype :
  sig
    val mk : string -> ?params:src_type list -> ?ctrs:src_type list -> unit ->
      datatype_declaration
  end

(** Effect interface *)
module EffInterface :
  sig
    val mk : string -> ?params:src_type list -> ?sigs:src_type list -> unit ->
      effect_interface
  end

(** Value declarations *)
module ValueDecl :
  sig
    val mk : string -> src_type -> value_declaration
  end

(** Type expressions *)
module Type :
  sig
    val mk : src_type_desc -> src_type

    val var : string -> src_type
    val arrow : src_type -> src_type -> src_type
    val constr : string -> src_type -> src_type
    val effect_sig : string -> src_type -> src_type
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
    val ctr : string -> ?pats:value_pattern list -> unit -> value_pattern
    val request : string -> ?pats:value_pattern list -> string ->
      computation_pattern
    val thunk : string -> computation_pattern
  end
