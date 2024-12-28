
open Instances

module Int = struct
  include Int

  let sum = List.fold_left ( + ) 0

  let prod = List.fold_left ( * ) 1
end

module Float = struct
  include Float

  let sum = List.fold_left ( +. ) 0.0

  let prod = List.fold_left ( *. ) 1.0
end

module List = struct
  include List

  (* Remove an element from a list. *)
  let remove element = filter ((<>) element)

  (* Remove each element in a list of elements from a list. *)
  let remove_all elements = fold_right remove elements

  let hd_opt l = match l with
    | [] -> None
    | (first :: _) -> Some first

  let tl_opt l = match l with 
    | [] -> None
    | (_ :: rest) -> Some rest
end

module Set = struct
  include Set

  module Make(Ord: OrderedType) = struct
    include Make(Ord)

    let for_all2 f s1 s2 =
      for_all (fun i1 -> for_all (f i1) s2) s1
  end
end

module Map = struct
  include Map

  module Make(Ord: OrderedType) = struct
    include Make(Ord)

    let keys s = to_list s |> List.map fst
    let values s = to_list s |> List.map snd
  end
end

module String = struct
  include String

  let words = Str.split (Str.regexp "[ \n\r\x0c\t]+")

  let lines = Str.split (Str.regexp "[\n\r\x0c]+")
end

let from_file update_from_string init file =
  let in_ch = open_in file in 
  let rec read_line rep =
    let line = try input_line in_ch with End_of_file -> "" in
    if line = "" then
      (close_in in_ch; rep)
    else
      read_line (update_from_string line rep)
  in read_line init


let lines_from_file = from_file (fun line rep -> rep @ [line]) []