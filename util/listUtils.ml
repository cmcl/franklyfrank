
let filter_map f xs =
  let ff x xs = match f x with
                | None -> xs
                | Some x -> x :: xs in
  List.fold_right ff xs []

let map = List.map

let zip = List.combine

let foldl = List.fold_left
