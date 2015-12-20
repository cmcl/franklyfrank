(***********************************************************************
 * Translate the untyped abstract syntax tree of the source language
 * into a untyped mid-level tree. The mid-level tree does not
 * distinguish between inferable and checkable values/computations.
 *
 *
 * Created by Craig McLaughlin on 21/07/2015.
 ***********************************************************************
 *)

open Show

type prog = tld list

and tld =
  | Mtld_datatype of datatype_declaration
  | Mtld_effin of effect_interface
  | Mtld_handler of handler_definition

and datatype_declaration = ParseTree.datatype_declaration

and effect_interface = ParseTree.effect_interface

and pattern = ParseTree.pattern

and src_type = ParseTree.src_type

and handler_definition =
  {
    mhdr_name : string;
    mhdr_type : src_type;
    mhdr_defs : handler_clause list
  }

and handler_clause = pattern list * mid_ccomputation

and mid_ccomputation =
  | Mccomp_cvalue of mid_cvalue
  | Mccomp_clauses of handler_clause list

and mid_cvalue =
  | Mcvalue_ivalue of mid_ivalue
  | Mcvalue_ctr of string * mid_cvalue list
  | Mcvalue_thunk of mid_ccomputation

and mid_ivalue =
  | Mivalue_var of string
  | Mivalue_cmd of string
  | Mivalue_int of int
  | Mivalue_float of float
  | Mivalue_bool of bool
  | Mivalue_str of string
  | Mivalue_icomp of mid_icomputation

and mid_icomputation =
  | Micomp_app of mid_ivalue * mid_ccomputation list
  | Micomp_let of string * mid_ccomputation * mid_ccomputation

(** Show functions for the tree (see also ParseTree module) *)
module ShowMidProg : SHOW with type t = prog

module ShowMidTLD : SHOW with type t = tld

module ShowMidHandler : SHOW with type t = handler_definition

module ShowHdrClause : SHOW with type t = handler_clause

module ShowMidCComp : SHOW with type t = mid_ccomputation

module ShowMidCValue : SHOW with type t = mid_cvalue

module ShowMidIValue : SHOW with type t = mid_ivalue

module ShowMidIComp : SHOW with type t = mid_icomputation
