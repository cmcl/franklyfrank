open Show
open ListUtils
open Utility

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
  | IValue_str of string
  (** Int/Bool/String literals *)
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
  | Spat_any (* [_] *)
  | Spat_thunk of string (* [t] for string t *)

and computation_pattern =
  | Scpat_request of string * value_pattern list * string

and value_pattern =
  | Svpat_any (* _ *)
  | Svpat_var of string
  | Svpat_int of int
  | Svpat_bool of bool
  | Svpat_str of string
   (** Int/Bool/String literals *)
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
    sei_commands : command_declaration list
  }

and command_declaration =
  {
    scmd_name : string;
    scmd_args : src_type list;
    scmd_res : src_type
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
  | Styp_datatype of string * src_type list
  | Styp_thunk of src_type
  | Styp_tvar of string (* user generated type variable *)
  | Styp_rtvar of src_tvar (* rigid (i.e. desugared user generated) type
			      variable *)
  | Styp_ftvar of src_tvar (* flexible (i.e. unification generated) type
			      variable *)
  | Styp_eff_set of src_type list (* set of effects: used for unifying
				     flexible effect sets *)
  | Styp_ref of (src_type Unionfind.point)
      (** Unification variable *)
(* Computations *)
  | Styp_comp of src_type list * src_type
(* Returners *)
  | Styp_ret of src_type list * src_type
(* Effect interfaces *)
  | Styp_effin of string * src_type list
(* Builtin types *)
  | Styp_bool
  | Styp_int
  | Styp_str

and src_tvar = string * int

let string_of_args sep ?(bbegin = true) ?(endd = false) f xs = match xs with
  | [] -> ""
  | xs -> (if bbegin then sep else "")
    ^ (String.concat sep (List.map f xs)) ^
    (if endd then sep else "")

let rec compare x y =
  let f acc x y =
    if acc != 0 then acc
    else compare x y in
  match x.styp_desc , y.styp_desc with
  | Styp_datatype (d, ps) , Styp_datatype (d', ps')
    -> let cmp = String.compare d d' in
       let cmp' = Pervasives.compare (length ps) (length ps') in
       if cmp != 0 then cmp
       else if cmp' != 0 then cmp'
       else foldl (uncurry @ f) 0 (zip ps ps')
  | Styp_datatype _ , _ -> 1
  | _               , Styp_datatype _ -> -1

  | Styp_thunk c, Styp_thunk c'  -> compare c c'
  | Styp_thunk _, _              -> 1
  | _           , Styp_thunk _   -> -1

  | Styp_tvar v, Styp_tvar v' -> String.compare v v'
  | Styp_tvar _, _            -> 1
  | _          , Styp_tvar _  -> -1

  | Styp_rtvar (_, n), Styp_rtvar (_, n')  -> Pervasives.compare n n'
  | Styp_rtvar _, _              -> 1
  | _           , Styp_rtvar _   -> -1

  | Styp_ftvar (_, n), Styp_ftvar (_, n')  -> Pervasives.compare n n'
  | Styp_ftvar _, _              -> 1
  | _           , Styp_ftvar _   -> -1

  | Styp_eff_set es, Styp_eff_set es'
    -> let cmp = Pervasives.compare (length es) (length es') in
       if cmp != 0 then cmp
       else foldl (uncurry @ f) 0 (zip es es')
  | Styp_eff_set _, _              -> 1
  | _             , Styp_eff_set _ -> -1

  | Styp_ref pt , Styp_ref pt'
    -> compare (Unionfind.find pt) (Unionfind.find pt)
  | Styp_ref pt  , _ -> compare (Unionfind.find pt) y
  | _           , Styp_ref pt -> compare x (Unionfind.find pt)

  | Styp_comp (ts, t), Styp_comp (ts', t')
    -> let cmp = compare t t' in
       let cmp' = Pervasives.compare (length ts) (length ts') in
       if cmp != 0 then cmp
       else if cmp' != 0 then cmp'
       else foldl (uncurry @ f) 0 (zip ts ts')
  | Styp_comp _      , _           -> 1
  | _                , Styp_comp _ -> -1

  | Styp_ret (ts, t) , Styp_ret (ts', t')
    -> let cmp = compare t t' in
       let cmp' = Pervasives.compare (length ts) (length ts') in
       if cmp != 0 then cmp
       else if cmp' != 0 then cmp'
       else foldl (uncurry @ f) 0 (zip ts ts')
  | Styp_ret _       , _          -> 1
  | _                , Styp_ret _ -> -1

  | Styp_effin (ei, ts), Styp_effin (ei', ts')
    -> let cmp = String.compare ei ei' in
       let cmp' = Pervasives.compare (length ts) (length ts') in
       if cmp != 0 then cmp
       else if cmp' != 0 then cmp'
       else foldl (uncurry @ f) 0 (zip ts ts')
  | Styp_effin _       , _            -> 1
  | _                  , Styp_effin _ -> -1

  | Styp_bool , Styp_bool -> 0
  | Styp_bool , _         -> 1
  | _         , Styp_bool -> -1

  | Styp_int , Styp_int -> 0
  | Styp_int , _         -> 1
  | _         , Styp_int -> -1

  | Styp_str  , Styp_str -> 0


(** Show functions *)
module ShowPattern : SHOW with type t = pattern = struct
  type t = pattern
  let rec show p = match p.spat_desc with
    | Spat_comp cp -> cshow cp
    | Spat_value vp -> vshow vp 
    | Spat_any -> "[_]"
    | Spat_thunk thk -> "[" ^ thk ^ "]"

  and cshow cp =
    match cp with
    | Scpat_request (c, ps, k)
      -> "[" ^ c ^ (string_of_args " " vshow ps) ^ " -> " ^ k ^ "]"

  and vshow vp =
    match vp with
    | Svpat_any -> "_"
    | Svpat_var v -> v
    | Svpat_int n -> string_of_int n
    | Svpat_bool b -> string_of_bool b
    | Svpat_str s -> "\"" ^ (String.escaped s) ^ "\""
    | Svpat_ctr (k, ps)
      -> "(" ^ k ^ (string_of_args " " vshow ps) ^ ")"
end

module rec ShowSrcType : SHOW with type t = src_type = struct
  type t = src_type
  let rec show typ = match typ.styp_desc with
    | Styp_tvar v -> v
    | Styp_rtvar (v,n) -> "r?" ^ v ^ (string_of_int n)
    | Styp_ftvar (v,n) -> "f?" ^ v ^ (string_of_int n)
    | Styp_eff_set es -> "[" ^ String.concat ", " (List.map show es) ^ "]"
    | Styp_bool -> "Bool"
    | Styp_comp (args, res)
      -> (string_of_args " -> " ~bbegin:false ~endd:true show args) ^ show res
    | Styp_datatype (k, ts)
      -> "(" ^ k ^ string_of_args " " show ts ^ ")"
    | Styp_effin (s, ts)
      -> s ^ " " ^ (String.concat " " (List.map show ts))
    | Styp_int -> "Int" 
    | Styp_ret (effs, res)
      -> "[" ^ (String.concat ", " (List.map show effs)) ^ "]" ^ (show res)
    | Styp_thunk c -> "{" ^ show c ^ "}"
    | Styp_ref t -> "|" ^ show (Unionfind.find t) ^ "|"
    | Styp_str -> "String"
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
      (String.concat "\n\t| " (List.map ShowCmd.show ei.sei_commands)) ^
      "\n"
end

and ShowCmd : SHOW with type t = command_declaration = struct
  type t = command_declaration
  let show cmd =
    cmd.scmd_name ^ " : " ^ 
      (match cmd.scmd_args with
      | [] -> ""
      | xs -> (String.concat " -> " (List.map ShowSrcType.show xs)) ^ " -> ")
    ^ (ShowSrcType.show cmd.scmd_res)
end
