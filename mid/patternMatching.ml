open List
open MidTree
open ParseTree
open ListUtils
open Utility

type 'a vector = 'a list
type 'a matrix = 'a vector vector
type action = MidTree.mid_ccomputation
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
| CseCtr of string * dtree
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

let string_of_patterns = string_of_args ", " ~bbegin:true string_of_pattern

let to_columns m =
  let cons = fun p ps -> p :: ps in
  let colgen (ps, a) (css, rs) =
    (map (uncurry cons) (combine ps css), a :: rs) in
  let rowlen = if length m > 0 then length (fst (hd m)) else 0 in
  fold_right colgen m (repeat [] rowlen,[])

let of_columns ps rs = combine (transpose ps) rs

let prmatrix m =
  let string_of_clause (ps, a) =
    (string_of_patterns ps) ^ " -> " ^ (ShowMidCComp.show a) in
  iter (fun c -> print_endline (string_of_clause c)) m

let specialise k n = []
(* TODO: Specialise on type signatures not ctrs. *)
  (* let clausegen k ps a = *)
  (*   match ps with *)
  (*   | p :: ps *)
  (*     -> begin match p.spat_desc with *)
  (*        | Spat_value vp *)
  (* 	   -> begin match vp with *)
  (*  	      | Svpat_any -> [(repeat (Pattern.any_value ()) n ++ ps, a)] *)
  (* 	      | Svpat_ctr (k', ps') when k = k' -> [(ps' ++ ps, a)] *)
  (* 	      | Svpat_var v -> [(repeat (Pattern.any_value ()) n ++ ps, a)] *)
  (*     	      end *)
  (* 	 | Spat_comp (Scpat_request (c, vs, r)) when c = k -> *)
  (* 	 | _ -> [] *)
  (*        end *)
  (*   | [] -> [] in *)
  (* flatten (map (fun (ps, a) -> clausegen c ps a) m) *)

let default m = []

(** Compute head values in patterns [ps]. *)
let compute_heads ps =
  let compute_hd p =
    match p.spat_desc with
    | Spat_value vp -> begin
                         match vp with
			 | Svpat_any
			 | Svpat_var _ (* FIXME: Possibly wrong. *)
			   -> TypeSigSet.singleton TSAllValues
			 | Svpat_ctr (k, _) -> TypeSigSet.singleton (TSCtr k)
			 | Svpat_int n -> TypeSigSet.singleton (TSInt n)
			 | Svpat_float f -> TypeSigSet.singleton (TSFloat f)
			 | Svpat_str s -> TypeSigSet.singleton (TSStr s)
			 | Svpat_bool b
			   -> let x = if b then TSTrue else TSFalse in
			      TypeSigSet.singleton x
                       end
    | Spat_comp (Scpat_request (c, _, _)) -> TypeSigSet.singleton (TSCmd c)
    | _ -> TypeSigSet.empty in
  foldl TypeSigSet.union TypeSigSet.empty (map compute_hd ps)

let matches m v = None

let eval_dtree vs t = Mccomp_cvalue (Mcvalue_ivalue (Mivalue_int 0))

let compile m = Fail
