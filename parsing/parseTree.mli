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
  | CComp_hdr_clause of pattern list * checkable_computation
  | CComp_emp_clause
  | CComp_compose of checkable_computation list

and checkable_value =
  | CValue_ivalue of inferable_value
  | CValue_ctr of string * checkable_value list
  | CValue_thunk of checkable_computation

and inferable_value =
  | IValue_ident of string
      (** Could be a monovar, polyvar or effect signature. *)
  | IValue_icomp of inferable_computation

and inferable_computation =
  | IComp_force of inferable_value
  | IComp_app of inferable_computation * checkable_computation

and pattern =
  {
    spat_desc : pattern_desc;
  }

and pattern_desc =
  | Spat_value of value_pattern
  | Spat_comp of computation_pattern

and computation_pattern =
  | Scpat_request of string * value_pattern list * string
  | Scpat_thunk of string

and value_pattern =
  | Svpat_var of string
  | Svpat_ctr of string * value_pattern list

and value_definition =
  {
    vdef_name : string;
    vdef_args : pattern list;
    vdef_comp : checkable_computation;
  }

and datatype_declaration =
  {
    sdt_name : string;
    sdt_parameters : src_type list;
    sdt_constructors : constructor_declaration list;
  }

and constructor_declaration =
  {
    sctr_name : string;
    sctr_args : src_type list;
    sctr_res : src_type
  }

and effect_interface = 
  {
    sei_name : string;
    sei_parameters: src_type list;
    sei_signatures : src_type list
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
  | Styp_constr of string * src_type
  | Styp_comp of src_type list * src_type
  | Styp_ctr of string * src_type list
  | Styp_effsig of string * src_type
  | Styp_effin of string * src_type list
  | Styp_ret of src_type list * src_type
  | Styp_thunk of src_type
