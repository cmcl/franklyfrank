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
    val value_decl : value_declaration -> term
    val value_defn : value_definition -> term
  end

(** Datatype declarations *)
module Datatype :
  sig
    val mk : string -> ?params:src_type list -> ?ctrs:src_type list -> unit ->
      datatype_declaration
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

  end

(** Value definitions *)
module ValueDefn :
  sig
    val mk : string -> ?args:string list -> checkable_computation ->
      value_definition
  end
