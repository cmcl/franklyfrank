let (@) f g = fun x -> f (g x)

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y
