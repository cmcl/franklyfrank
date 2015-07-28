module type SHOW = sig
  type t
  val show : t -> string
end

module ShowList (X : SHOW) : SHOW with type t = X.t list = struct
  type t = X.t list
  let show xs = String.concat "\n" (List.map X.show xs) ^ "\n"
end
