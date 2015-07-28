open ParseTree
open Show

type prog = tld list

and tld =
  | Mtld_datatype of datatype_declaration
  | Mtld_effin of effect_interface
  | Mtld_handler of handler_definition

and handler_definition =
  {
    mhdr_name : string;
    mhdr_type : src_type;
    mhdr_defs : handler_clause list
  }

and handler_clause = pattern list * mid_ccomputation

and mid_ccomputation =
  | Mccomp_cvalue of mid_cvalue
  | Mccomp_clause of mid_comp_clause

and mid_comp_clause =
  | Mcomp_clauses of handler_clause list
  | Mcomp_emp_clause

and mid_cvalue =
  | Mcvalue_ivalue of mid_ivalue
  | Mcvalue_ctr of string * mid_cvalue list
  | Mcvalue_thunk of mid_ccomputation

and mid_ivalue =
  | Mivalue_var of string
  | Mivalue_sig of string
  | Mivalue_icomp of mid_icomputation

and mid_icomputation =
  | Micomp_force of mid_ivalue
  | Micomp_app of mid_ivalue * mid_ccomputation list

module MidTLD : SHOW with type t = tld = struct
  type t = tld
  let show d = "EMPTY"
end

module MidProg : SHOW with type t = prog = ShowList(MidTLD)



