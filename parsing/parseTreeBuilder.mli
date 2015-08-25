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
      (** Compose a (possible empty) list of clauses. *)
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
    val integer : int -> inferable_value
    val boolean : bool -> inferable_value
    val icomp : inferable_computation -> inferable_value
  end

(** Inferable computation *)
module IComp :
  sig
    val app : inferable_value -> ?args:checkable_computation list -> unit ->
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
    val mk : string -> ?params:src_type list ->
      ?cmds:command_declaration list -> unit -> effect_interface

    val cmd_decl : string -> ?args:src_type list -> src_type ->
      command_declaration
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

    (* Value type constructors *)
    val rigid_tvar : string -> src_type
    val flexi_tvar : string -> src_type
    val datatype : string -> src_type list -> src_type
    val sus_comp : src_type -> src_type

    val comp : ?args:src_type list -> src_type -> src_type
    (** Construct a computation type. *)

    val returner : src_type -> ?effs:src_type list -> unit -> src_type
    (** Construct a returner type *)

    val effin : string -> ?params:src_type list -> unit -> src_type
    (** Construct an effect interface. *)

    val bool : unit -> src_type
    val int : unit -> src_type

    (* Effect sets *)
    val effect_var_set : src_type list
    val closed_effect_set : src_type list
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
    val any : unit -> pattern
      (** Unnamed thunk *)
    val thunk : string -> pattern
      (** Named thunk *)

    val any_value : unit -> value_pattern
    val var : string -> value_pattern
    val integer : int -> value_pattern
    val boolean : bool -> value_pattern
    val ctr : string -> ?pats:value_pattern list -> unit ->
      value_pattern

    val request : string -> ?pats:value_pattern list -> string ->
      computation_pattern
  end
