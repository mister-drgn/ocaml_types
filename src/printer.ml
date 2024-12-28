module type Stringer = sig
  type t

  val to_string: t -> string
end

module type OrderedStringer = sig
  include Set.OrderedType

  val to_string: t -> string
end

module type S = sig
  include Stringer

  val print: t -> unit
  val print_endline: t -> unit
end

module Make (Str: Stringer) = struct
  include Str

  let print s = to_string s |> print_string

  let print_endline s = to_string s |> print_endline
end

