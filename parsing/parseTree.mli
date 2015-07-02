(***********************************************************************
 * Untyped Abstract Syntax Tree for the Frank source language.
 * 
 *
 * Created by Craig McLaughlin on 30/06/2015.
 ***********************************************************************
 *)

type prog = term list

and term =
  | Term_decl of src_declaration
  | Term_defn of src_definition

and src_declaration =
  | Decl_datatype of datatype_declaration
  | Decl_effin of effect_interface
  | Decl_value of value_declaration

and checkable_computation =
  | CComp_cvalue of checkable_value
  | CComp_hdr_clause
  | CComp_emp_clause
  | CComp_compose

and checkable_value =
  | CValue_ivalue of inferable_value
  | CValue_construct of string * checkable_value list
  | CValue_thunk of checkable_computation

and inferable_value =
  | IValue_monovar of string
  | IValue_polyvar of string
  | IValue_effsig of string
  | IValue_icomp

and src_definition =
  {
    name : string;
    args : string list;
    comp : checkable_computation;
  }

and datatype_declaration =
  {
    name : string;
    parameters : string list;
    constructors : constructor_declaration list;
  }

and effect_interface = 
  {
    name : string
    parameters: string list;
    signatures : string;
  }

and value_declaration =
  {
    valdec_name : string;
    valdec_type : src_type;
  }

and constructor_declaration =
  {
    cstr_name : string;
    cstr_type : src_type;
  }

and src_type =
  {
    typ_desc : src_type_desc
  }

and src_type_desc =
  | typ_var of string (* type variable *)
  | typ_arrow of src_type * src_type
  | typ_cstr of string * src_type list
