
let do_debug = ref false

let debug_flag b = do_debug := b

let print fmt =
  let f = if !do_debug then Printf.fprintf else Printf.ifprintf in
  f stderr fmt
