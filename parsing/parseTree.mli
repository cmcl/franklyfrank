(***********************************************************************
 * Untyped Abstract Syntax Tree for the Frank source language.
 * 
 *
 * Created by Craig McLaughlin on 30/06/2015.
 ***********************************************************************
 *)

type prog = term list

and term =
  | Sterm_datatype of datatype_declaration
  | Sterm_effin of effect_interface
  | Sterm_vdecl of value_declaration
  | Sterm_vdefn of value_definition

and checkable_computation =
  | CComp_cvalue of checkable_value
  | CComp_hdr_clause
  | CComp_emp_clause
  | CComp_compose

and checkable_value =
  | CValue_ivalue of inferable_value
  | CValue_ctr of string * checkable_value list
  | CValue_thunk of checkable_computation

and inferable_value =
  | IValue_monovar of string
  | IValue_polyvar of string
  | IValue_effsig of string
  | IValue_icomp

and value_definition =
  {
    vdef_name : string;
    vdef_args : string list;
    vdef_comp : checkable_computation;
  }

and datatype_declaration =
  {
    sdt_name : string;
    sdt_parameters : src_type list;
    sdt_constructors : src_type list;
  }

and effect_interface = 
  {
    sei_name : string;
    sei_parameters: string list;
    sei_signatures : string;
  }

and value_declaration =
  {
    svdecl_name : string;
    svdecl_type : src_type;
  }

and src_type =
  {
    styp_desc : src_type_desc
  }

and src_type_desc =
  | Styp_var of string (* type variable *)
  | Styp_arrow of src_type * src_type
  | Styp_ctr of string * src_type
