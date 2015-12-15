let (++) = List.append
let hd = List.hd
let tl = List.tl

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

let rec transpose xss =
  match xss with
  |           []     -> []
  |    []     :: xss -> transpose xss
  | (x :: xs) :: xss -> (x :: (map hd xss)) :: (transpose (xs :: map tl xss))
