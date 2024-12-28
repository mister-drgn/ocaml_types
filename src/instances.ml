module List = struct
  include List

  module Class = Typeclasses.DoMonadPlus(struct
    include List
    type ('a, 'e) c = 'a list

    let bind l f = concat_map f l

    let empty = []

    let mplus = ( @ )
    
    let map = map

    let return x = [x] 

    let apply fs xs =
      concat_map (fun f -> map f xs) fs

  end)

  module OptionT = Transformers.DoOptionT(Class)

  module MakePrinter(S: Printer.Stringer) = Printer.Make(struct
    type t = S.t list

    let to_string l = map S.to_string l |>
      String.concat "; " |>
      Printf.sprintf "[%s]"
  end)

end

let _ = List.Class.return

let (x: int List.t) = List.Class.(((+) 1) <$> [3])

module Seq = struct
  include Seq

  module Class = Typeclasses.DoMonadPlus(struct
    type ('a, 'e) c = 'a t
    
    let bind seq f = flat_map f seq

    let empty = empty

    let mplus = append

    let map = map
    
    let return = return

    let apply fs xs =
      flat_map (fun f -> map f xs) fs
  end)

  module OptionT = Transformers.DoOptionT(Class)

  module ZipSeq = Typeclasses.DoApplicative(struct
    type ('a, 'e) c = 'a t

    let map = map
    
    let return = repeat

    let apply fs xs = map2 ( @@ ) fs xs
  end)
end

module Option = struct
  include Option

  module Class = Typeclasses.DoMonadPlus(struct
    include Option
    type ('a, 'e) c = 'a option

    let bind = bind

    let empty = None

    let mplus x y =
      match x, y with
      | Some x, _ -> Some x
      | _, Some y -> Some y
      | _, _      -> None
    
    let map = map

    let return x = Some x

    let apply f x =
      match f, x with
      | Some f, Some x  -> Some (f x)
      | _               -> None
  end)

  module ToStringer(S: Printer.Stringer) = Printer.Make(struct
    type t = S.t option

    let to_string o = match o with 
    | Some x -> S.to_string x |> Printf.sprintf "optional(%s)"
    | None   -> "nil"
  end)

end

module String = struct
  include String

  module Class = Mono_typeclasses.DoMonoMonadPlus(struct
    include String
    type c = string
    type elem = char

    let bind str f =
      to_seq str |>
      Seq.map f |>
      List.of_seq |>
      concat ""

    let empty = empty

    let mplus = ( ^ )
    
    let map = map
    let return x = make 1 x
  end)

  let to_string (s: string) = s
end

module Set = struct
  include Set

  module Make(Ord: OrderedType) = struct
    include Make(Ord)

    module Class = Mono_typeclasses.DoMonoMonadPlus(struct
      type c = t
      type elem = Ord.t

      let bind set f =
        to_seq set |>
        Seq.map f |>
        Seq.fold_left union empty

      let empty = empty

      let mplus = union

      let map = map

      let return = singleton
    end)
  end

  module MakePrinter(S: Printer.OrderedStringer) = Printer.Make(struct
    module MySet = Make(S)
    type t = MySet.t

    let to_string s = MySet.to_list s |> List.map S.to_string |>
      String.concat "; " |>
      Printf.sprintf "[%s]"
  end)

end

module Map = struct
  include Map

  module Make(Ord: OrderedType) = struct
    include Make(Ord)

    module Class = Typeclasses.DoFunctor(struct
      type ('a, 'e) c = 'a t

      let map = map
    end)
  end

  module ToStringer(K: Printer.OrderedStringer) (V: Printer.Stringer) = Printer.Make(struct
    module MyMap = Make(K)
    type t = V.t MyMap.t

    let to_string m = MyMap.to_list m |>
      List.map (fun (k,v) -> Printf.sprintf "%s: %s" (K.to_string k) (V.to_string v)) |>
      String.concat "; " |>
      Printf.sprintf "[%s]"
  end)
end

module Result = struct
  include Result

  module Class = Typeclasses.DoMonad(struct
    include Result
    type ('a, 'e) c = ('a, 'e) result

    let map = map

    let apply f x =
      match f, x with
      | Ok f, Ok x  -> Ok (f x)
      | Error f, _  -> Error f
      | _, Error x  -> Error x

    let return x = Ok x

    let bind = bind
  end)

  module OptionT = Transformers.DoOptionT(Class)
end

module Fun = struct
  include Fun

  module Class = Typeclasses.DoFunctor(struct
    type ('a, 'e) c = 'e -> 'a

    let map f g x = f(g(x))

    (* let return x = fun _ -> x

    let apply f g x =  f x g(x) *)
  end)
end

let r = ((+) 3)

let q = Fun.Class.((Int.to_string) <$> ((+) 3))


let comp xs = List.Class.(
  let* x = xs
  and* y = xs in
    return (x + y)
)

let addEm xs = List.Class.(
  ( + ) <$> xs <*> [4; 5; 6]
)

let zeroEm xs = List.Class.(
  0 <$ xs
)

let testSeq xs = Seq.Class.(
  let xs = List.to_seq xs in
  let* x = xs
  and* y = xs in
  return (x, y)
)