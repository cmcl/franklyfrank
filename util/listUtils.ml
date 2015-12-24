
let (++) = List.append
let hd = List.hd
let tl = List.tl
let find = List.find

let rec repeat x n = if n <= 0 then [] else x :: (repeat x (n-1))

let filter_map f xs =
  let ff x xs = match f x with
                | None -> xs
                | Some x -> x :: xs in
  List.fold_right ff xs []

let map = List.map

let zip = List.combine

let foldl = List.fold_left

let length = List.length

(** Haskell has a better implementation: Combination of State monad
    and Traversable functor. TODO: Investigate such a generalisation. *)
let rec map_accum f a xs =
  match xs with
  | [] -> (a, [])
  | x :: xs' -> let (a', x') = f a x in
		let (a'', xs'') = map_accum f a' xs' in
		(a'', x' :: xs'')

let swap zs i j =
  let rec swap' x y zs =
    match zs with
    | [] -> []
    | (n, z) :: zs -> (if i = n then y
                       else if j = n then x
		       else z) :: (swap' x y zs) in
  let (_, zs) = map_accum (fun i z -> (i+1, (i, z))) 0 zs in
  let (_, x) = find (fun (n, _) -> n = i) zs in
  let (_, y) = find (fun (n, _) -> n = j) zs in
  swap' x y zs

let rec transpose xss =
  match xss with
  |           []     -> []
  |    []     :: xss -> transpose xss
  | (x :: xs) :: xss -> (x :: (map hd xss)) :: (transpose (xs :: map tl xss))
