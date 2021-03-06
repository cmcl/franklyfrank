open ParseTree
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
      deriving (Show)

module rec ShowMidProg : SHOW
  with type t = prog = ShowList(ShowMidTLD)

and ShowMidTLD : SHOW with type t = tld = struct
  type t = tld
  let show d = match d with
    | Mtld_datatype dt -> ShowDatatype.show dt
    | Mtld_effin ei -> ShowEffin.show ei
    | Mtld_handler hdr -> ShowMidHandler.show hdr
end

and ShowMidHandler : SHOW with type t = handler_definition = struct
  type t = handler_definition

  let show_cse name (ps, cc) =
    name ^ (string_of_args " " ShowPattern.show ps) ^ " = " ^
      ShowMidCComp.show cc

  let show h =
    "{- START OF HANDLER " ^ h.mhdr_name ^ " DEFINITION -}\n" ^
    h.mhdr_name ^ " : " ^ (ShowSrcType.show h.mhdr_type) ^ "\n" ^
    (String.concat "\n" (List.map (show_cse h.mhdr_name) h.mhdr_defs)) ^
    "\n{- END OF HANDLER " ^ h.mhdr_name ^ " DEFINITION -}\n"
end

and ShowHdrClause : SHOW with type t = handler_clause = struct
  type t = handler_clause
  let show (ps, cc) =
    String.concat " " (List.map ShowPattern.show ps) ^ " = " ^
      ShowMidCComp.show cc
end

and ShowMidCComp : SHOW with type t = mid_ccomputation = struct
  type t = mid_ccomputation
  let show c = match c with
    | Mccomp_cvalue cv -> ShowMidCValue.show cv
    | Mccomp_clauses cses -> if cses = [] then "()" else ShowClauses.show cses
end

and ShowMidCValue : SHOW with type t = mid_cvalue = struct
  type t = mid_cvalue
  let rec show cv = match cv with
    | Mcvalue_ivalue iv -> ShowMidIValue.show iv
    | Mcvalue_ctr (k, vs)
      -> "(" ^ k ^ (string_of_args " " ShowMidCValue.show vs) ^ ")"
    | Mcvalue_thunk cc -> "{" ^ ShowMidCComp.show cc ^ "}"
end

and ShowMidIValue : SHOW with type t = mid_ivalue = struct
  type t = mid_ivalue
  let show iv = match iv with
    | Mivalue_var v -> "({-VAR-} " ^ v ^ ")"
    | Mivalue_cmd c -> "({-CMD-} " ^ c ^ ")"
    | Mivalue_int n -> "({-INT-} " ^ string_of_int n ^ ")"
    | Mivalue_float f -> "({-FLOAT-} " ^ string_of_float f ^ ")"
    | Mivalue_bool b -> "({-BOOL-} " ^ string_of_bool b ^ ")"
    | Mivalue_str s -> "({-STRING-} \"" ^ (String.escaped s) ^ "\")"
    | Mivalue_icomp ic -> ShowMidIComp.show ic
end

and ShowMidIComp : SHOW with type t = mid_icomputation = struct
  type t = mid_icomputation
  let show ic = match ic with
    | Micomp_app (iv, xs)
      -> "({-APP-}" ^ (ShowMidIValue.show iv) ^ "!" ^
           (string_of_args " " ShowMidCComp.show xs) ^ ")"
    | Micomp_let (x, cc1, cc2)
      -> "(let " ^ x ^ " = " ^ (ShowMidCComp.show cc1) ^ " in " ^
      (ShowMidCComp.show cc2) ^ ")"
end

and ShowClauses : SHOW
  with type t = handler_clause list = ShowList(ShowHdrClause)
