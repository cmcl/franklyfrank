(***********************************************************************
 * Collection of helper functions to construct components of the
 * parse tree (untyped AST). Inspired by the frontend design of the
 * OCaml compiler.
 * 
 *
 * Created by Craig McLaughlin on 30/06/2015.
 ***********************************************************************
 *)

(** Datatype declarations *)
module Datatype :
  sig
    val mk : string -> ?params:src_type list -> ?cstrs:src_type list
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
  end

(** Value definitions *)
module ValueDefn :
  sig
    val mk : string -> ?args:string list -> checkable_computation ->
      src_definition
  end
