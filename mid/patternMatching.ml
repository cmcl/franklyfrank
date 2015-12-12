open List
open MidTree

type 'a vector = 'a list
type 'a matrix = 'a vector vector
type action = MidTree.mid_ccomputation
type pattern = MidTree.pattern
type value = MidTree.mid_cvalue
(** Helpful synonyms. *)

type clause = pattern vector * action
type pmatrix = pattern matrix
type cmatrix = clause matrix
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
    deriving (Show)
(** Cases which occur at multi-way test nodes within a decision tree. *)

let rec is_inst p v = false

and is_inst_vec ps vs =
  fold_left (&&) true (map (uncurry is_inst) (combine ps vs))

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
    (string_of_patterns ps) ^ " -> " ^ (string_of_int a) in
  iter (fun c -> print_endline (string_of_clause c)) m

let specialise c n m = []

let default m = []

let matches m v = None

let eval_dtree vs t = Mccomp_cvalue (Mcvalue_ivalue (Mivalue_int 0))

let compile m = Fail
