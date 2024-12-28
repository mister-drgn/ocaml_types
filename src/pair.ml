let fst (f,_) = f

let snd (_, s) = s

module MakePrinter(Fst: Printer.Stringer) (Snd: Printer.Stringer) = Printer.Make(struct
  type t = Fst.t * Snd.t

  let to_string (f, s) = Printf.sprintf "(%s, %s)" (Fst.to_string f) (Snd.to_string s)
end)