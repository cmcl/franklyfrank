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
  | Styp_var of string (* type variable *)
  | Styp_arrow of src_type * src_type
  | Styp_constr of string * src_type
  | Styp_comp of src_type list * src_type
  | Styp_ctr of string * src_type list
  | Styp_effin of string * src_type list
  | Styp_ret of src_type list * src_type
  | Styp_thunk of src_type


let string_of_args sep ?(bbegin = true) ?(endd = false) f xs = match xs with
  | [] -> ""
  | xs -> (if bbegin then sep else "")
    ^ (String.concat sep (List.map f xs)) ^
    (if endd then sep else "")

(** Show functions *)
module ShowPattern : SHOW with type t = pattern = struct
  type t = pattern
  let rec show p = match p.spat_desc with
    | Spat_comp cp -> cshow cp
    | Spat_value vp -> vshow vp 
    | Spat_any -> "_"

  and cshow cp =
    match cp with
    | Scpat_request (c, ps, k)
      -> "[" ^ c ^ (string_of_args " " vshow ps) ^ " -> " ^ k ^ "]"
    | Scpat_thunk x -> x ^ "!"

  and vshow vp =
    match vp with
    | Svpat_var v -> v
    | Svpat_int n -> string_of_int n
    | Svpat_bool b -> string_of_bool b
    | Svpat_ctr (k, ps)
      -> "(" ^ k ^ (string_of_args " " vshow ps) ^ ")"
end

module rec ShowSrcType : SHOW with type t = src_type = struct
  type t = src_type
  let rec show typ = match typ.styp_desc with
    | Styp_var v -> v
    | Styp_arrow (a, b) -> (show a) ^ " -> " ^ show b
    | Styp_constr (k, v) -> k ^ " :\t" ^ (show v)
    | Styp_comp (args, res)
      -> (string_of_args " -> " ~bbegin:false ~endd:true show args) ^ show res
    | Styp_ctr (k, ts)
      -> "(" ^ k ^ string_of_args " " show ts ^ ")"
    | Styp_effin (s, ts)
      -> s ^ " " ^ (String.concat " " (List.map show ts))
    | Styp_ret (effs, res)
      -> "[" ^ (String.concat ", " (List.map show effs)) ^ "]" ^ (show res)
    | Styp_thunk c -> "{" ^ show c ^ "}"
end

and ShowDatatype : SHOW with type t = datatype_declaration = struct
  type t = datatype_declaration
  let show dt =
    "data " ^ dt.sdt_name ^ " " ^
      (String.concat " " (List.map ShowSrcType.show dt.sdt_parameters)) ^
      " = " ^
      (String.concat "\n\t| " (List.map ShowCtr.show dt.sdt_constructors)) ^
      "\n"
end

and ShowCtr : SHOW with type t = constructor_declaration = struct
  type t = constructor_declaration
  let show ctr =
    ctr.sctr_name ^ " : " ^ 
      (match ctr.sctr_args with
      | [] -> ""
      | xs -> (String.concat " -> " (List.map ShowSrcType.show xs)) ^ " -> ")
    ^ (ShowSrcType.show ctr.sctr_res)
end

and ShowEffin : SHOW with type t = effect_interface = struct
  type t = effect_interface
  let show ei = 
    "interface " ^ ei.sei_name ^ " " ^
      (String.concat " " (List.map ShowSrcType.show ei.sei_parameters)) ^
      " = " ^
      (String.concat "\n\t| " (List.map ShowSig.show ei.sei_signatures)) ^
      "\n"
end

and ShowSig : SHOW with type t = signature_declaration = struct
  type t = signature_declaration
  let show si =
    si.ssig_name ^ " : " ^ 
      (match si.ssig_args with
      | [] -> ""
      | xs -> (String.concat " -> " (List.map ShowSrcType.show xs)) ^ " -> ")
    ^ (ShowSrcType.show si.ssig_res)
end
