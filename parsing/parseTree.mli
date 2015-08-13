(***********************************************************************
 * Untyped Abstract Syntax Tree for the Frank source language.
 * 
 *
 * Created by Craig McLaughlin on 30/06/2015.
 ***********************************************************************
 *)

open Show

type prog = term list

and term =
  | Sterm_datatype of datatype_declaration
  | Sterm_effin of effect_interface
  | Sterm_vdecl of value_declaration
  | Sterm_vdefn of value_definition

and checkable_computation =
  | CComp_cvalue of checkable_value
  | CComp_hdr_clause of pattern list * checkable_computation
  | CComp_compose of checkable_computation list

and checkable_value =
  | CValue_ivalue of inferable_value
  | CValue_ctr of string * checkable_value list
  | CValue_thunk of checkable_computation

and inferable_value =
  | IValue_ident of string
      (** Could be a monovar, polyvar or effect signature. *)
  | IValue_int of int
  | IValue_bool of bool
  (** Int/Bool literals *)
  | IValue_icomp of inferable_computation

and inferable_computation =
  | IComp_app of inferable_value * checkable_computation list

and pattern =
  {
    spat_desc : pattern_desc;
  }

and pattern_desc =
  | Spat_value of value_pattern
  | Spat_comp of computation_pattern
  | Spat_any

and computation_pattern =
  | Scpat_request of string * value_pattern list * string
  | Scpat_thunk of string

and value_pattern =
  | Svpat_var of string
  | Svpat_int of int
  | Svpat_bool of bool
   (** Int/Bool literals *)
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
    sei_signatures : signature_declaration list
  }

and signature_declaration =
  {
    ssig_name : string;
    ssig_args : src_type list;
    ssig_res : src_type
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
(* Values *)
  | Styp_ctr of string * src_type list
  | Styp_thunk of src_type
  | Styp_rtvar of int (* rigid (i.e. user generated) type variable *)
  | Styp_ftvar of int (* flexible (i.e. unification generated) type
			 variable *)
(* Computations *)
  | Styp_comp of src_type list * src_type
(* Returners *)
  | Styp_ret of src_type list * src_type
(* Effect interfaces *)
  | Styp_effin of string * src_type list
(* Builtin types *)
  | Styp_bool
  | Styp_int

(** Show functions *)
val string_of_args : string -> ?bbegin:bool -> ?endd:bool ->
  ('a -> string) -> 'a list -> string

module ShowPattern : SHOW with type t = pattern

module ShowSrcType : SHOW with type t = src_type

module ShowDatatype : SHOW with type t = datatype_declaration

module ShowEffin : SHOW with type t = effect_interface
