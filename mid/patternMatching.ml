open List
open MidTree
open MidTyping
open ParseTree
open ParseTreeBuilder
open ListUtils
open Utility

type 'a vector = 'a list
type 'a matrix = 'a vector vector
type action = MidTree.mid_ccomputation deriving (Show)
type pattern = MidTree.pattern
type value = MidTree.mid_cvalue
(** Helpful synonyms. *)

type clause = pattern vector * action
type pmatrix = pattern matrix
type cmatrix = clause vector
(** Shorthands. *)

type dtree =
  Fail
| Leaf of action
| Swap of int * dtree (* Subterm to be inspected w.r.t the decision tree. *)
| Switch of case list
(** Representation for decision trees the target of pattern matching
    compilation. *)

and case =
  CseDefault of dtree
| CseSig of MidTyping.type_sig * dtree
    deriving (Show)
(** Cases which occur at multi-way test nodes within a decision tree. *)

let foldmapb f xs = fold_left (&&) true (map f xs)

let rec is_inst p v =
  match p.spat_desc, v with
  | Spat_value vp, Mcvalue_ivalue _
  | Spat_value vp, Mcvalue_ctr _ -> is_value_inst vp v
  | _, _ -> false (* Computations not supported yet. *)

and is_value_inst vp cv =
  match vp, cv with
  | Svpat_any, _
  | Svpat_var _, _ -> true
  | Svpat_ctr (k, ps), Mcvalue_ctr (k', vs) when k = k'
    -> foldmapb (uncurry is_value_inst) (combine ps vs)
  | _, Mcvalue_ivalue iv
    -> begin
         match vp, iv with
	 | Svpat_int x, Mivalue_int y -> x = y
	 | Svpat_float x, Mivalue_float y -> x = y
	 | Svpat_bool x, Mivalue_bool y -> x = y
	 | Svpat_str x, Mivalue_str y -> x = y
	 | _ -> false
       end
  | _ -> false

and is_inst_vec ps vs = foldmapb (uncurry is_inst) (combine ps vs)

let not_inst p v = not (is_inst p v)

let string_of_pattern = ShowPattern.show

let string_of_patterns = string_of_args ", " ~bbegin:false string_of_pattern

let to_columns m =
  let cons = fun p ps -> p :: ps in
  let colgen (ps, a) (css, rs) =
    (map (uncurry cons) (combine ps css), a :: rs) in
  let rowlen = if length m > 0 then length (fst (hd m)) else 0 in
  fold_right colgen m (repeat [] rowlen,[])

let of_columns ps rs = combine (transpose ps) rs

let get_pmatrix = transpose @ fst @ to_columns

let prpatmatrix m =
  iter (fun ps -> print_endline (string_of_patterns ps)) m

let prmatrix m =
  let string_of_clause (ps, a) =
    (string_of_patterns ps) ^ " -> " ^ (ShowMidCComp.show a) in
  iter (fun c -> print_endline (string_of_clause c)) m

(** Specialises the matrix [m] using the specialisation function [specf]. If
    [specf] returns [(true, xs)] for some list [xs] then the function
    generates a row with [xs] prepended to the remaining patterns of the
    row. If [specf] returns [(false, xs)] for some list [xs] the list is
    ignored and no row is produced. *)
let specialise_using_fun specf m =
  let clausegen (ps, a) =
    match ps with
    | p :: ps -> let (b, xs) = specf p in
		 if b then [(xs ++ ps, a)] else []
    | [] -> [] in
  flatten (map clausegen m)

let specialise tsg m =
  (* The following conditions are true for all value type signatures with
     arity n. *)
  let defaults_for_values p n =
    match p.spat_desc with
    | Spat_value Svpat_any
    | Spat_value (Svpat_var _)
    | Spat_any
    | Spat_thunk _ -> (true, repeat (Pattern.vpat (Pattern.any_value ())) n)
    | _ -> (false, []) in
  match tsg with
  | TSAllValues -> let val_spec p = defaults_for_values p 0 in
		   specialise_using_fun val_spec m
  | TSBool b -> let bool_spec p =
		  match p.spat_desc with
		  | Spat_value (Svpat_bool b') -> (b = b', [])
		  | _ -> defaults_for_values p 0 in
		specialise_using_fun bool_spec m
  | TSFloat f -> let float_spec p =
		   match p.spat_desc with
		   | Spat_value (Svpat_float f') -> (f = f', [])
		   | _ -> defaults_for_values p 0 in
		 specialise_using_fun float_spec m
  | TSInt n -> let int_spec p =
		 match p.spat_desc with
		 | Spat_value (Svpat_int n') -> (n = n', [])
		 | _ -> defaults_for_values p 0 in
	       specialise_using_fun int_spec m
  | TSStr s -> let str_spec p =
		 match p.spat_desc with
		 | Spat_value (Svpat_str s') -> (s = s', [])
		 | _ -> defaults_for_values p 0 in
	       specialise_using_fun str_spec m
  | TSCtr (k, n) -> let ctr_spec p =
		      match p.spat_desc with
		      | Spat_value (Svpat_ctr (k', ps))
			-> (k = k', map Pattern.vpat ps)
		      | _ -> defaults_for_values p n in
		    specialise_using_fun ctr_spec m
  | TSCmd (c, n) -> let cmd_spec p =
		      match p.spat_desc with
		      | Spat_comp (Scpat_request (c', vs, r))
			-> let p = Pattern.vpat (Pattern.var r) in
			   let vs' = map Pattern.vpat vs in
			   (c = c', vs' ++ [p])
		      | _ -> (false, []) in
		    specialise_using_fun cmd_spec m

let default m =
  let clausegen (ps, a) =
    match ps with
    | p :: ps
      -> begin match p.spat_desc with
         | Spat_value _
	 | Spat_comp _ -> []
	 | Spat_any
	 | Spat_thunk _ -> [(ps, a)]
         end
    | [] -> [] in
  flatten (map clausegen m)

let rec all_wild ps =
  match ps with
  | p :: ps -> begin match p.spat_desc with
               | Spat_any
	       | Spat_thunk _ -> all_wild ps
	       | _ -> false
               end
  | [] -> true (* No columns case *)

(* Pick the first column which has a pattern that is not a wildcard.
   Return both the column number, the column and the remaining columns. *)
let pick_column css =
  let rec pick n css =
    match css with
    | [] -> failwith "invariant invalidated"
    | cs :: css -> if not (all_wild cs) then (n, cs)
                   else pick (n+1) css in
  pick 0 css

(** Compute head type signatures in patterns [ps]. *)
let compute_heads ps =
  let compute_hd p =
    match p.spat_desc with
    | Spat_value vp -> begin
                         match vp with
			 | Svpat_any
			 | Svpat_var _
			   -> TypeSigSet.singleton TSAllValues
			 | Svpat_ctr (k, vs)
			   -> TypeSigSet.singleton (TSCtr (k, length vs))
			 | Svpat_int n -> TypeSigSet.singleton (TSInt n)
			 | Svpat_float f -> TypeSigSet.singleton (TSFloat f)
			 | Svpat_str s -> TypeSigSet.singleton (TSStr s)
			 | Svpat_bool b -> TypeSigSet.singleton (TSBool b)
                       end
    | Spat_comp (Scpat_request (c, vs, _))
      -> TypeSigSet.singleton (TSCmd (c, length vs + 1))
    | _ -> TypeSigSet.empty in
  foldl TypeSigSet.union TypeSigSet.empty (map compute_hd ps)

let matches vs m =
  let matches_row j vs ps =
    if is_inst_vec ps vs then Some j else None in
  let find_match a ps =
    match a with
    | (Some _, _) -> a (* Exit as soon as a match is found. *)
    | (None, j) -> (matches_row j vs ps, j+1) in
  match foldl find_match (None, 0) m with
  | (Some j, _) -> Some j
  | (None, _) -> None

let eval_dtree vs t = Mccomp_cvalue (Mcvalue_ivalue (Mivalue_int 0))

let rec compile m =
  let make_case tsg =
    let tree = compile (specialise tsg m) in
    CseSig (tsg, tree) in
  match m with
  | [] -> Fail (* No row case *)
  | (ps, a) :: m' when all_wild ps -> Leaf a (* Default case is first row *)
  | _
    -> let (css, _) = to_columns m in
       let (i, cs) = pick_column css in
       if i != 0 then
	 let (pm,rs) = split m in
	 let pm' = swap (transpose pm) 0 i in
	 let m' = combine (transpose pm') rs in
	 Swap (i, compile m')
       else
	 let hs = compute_heads cs in
         (* Compute decision tree for each signature appearing in column. *)
	 let cases = map make_case (TypeSigSet.elements hs) in
	 let d = CseDefault (compile (default m)) in
	 Switch (cases ++ [d])
