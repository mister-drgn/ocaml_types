module type ModuleInstance = sig
  type ('a, 'e) c

  val map: ('a -> 'b) -> ('a, 'e) c -> ('b, 'e) c

  val apply : (('a -> 'b), 'e) c -> ('a, 'e) c -> ('b, 'e) c

  val bind: ('a, 'e) c -> ('a -> ('b, 'e) c) -> ('b, 'e) c

  val return: 'a -> ('a, 'e) c

  val ( let* ): ('a, 'e) c -> ('a -> ('b, 'e) c) -> ('b, 'e) c
end

module DoOptionT(M: ModuleInstance) = struct

  include Typeclasses.DoMonad(struct
    type ('a, 'e) c = ('a option, 'e) M.c

    let map f x = M.(
      let* maybe_val = x in
      match maybe_val with
      | None -> return None
      | Some real_val -> Some (f real_val) |> return
    )

    let apply f x = M.(
      let* maybe_f = f in
      let* maybe_x = x in
      match maybe_f, maybe_x with
      | Some f, Some x -> Some (f x) |> return
      | _              -> return None
    )

    let return x = Some x |> M.return

    let bind x f = M.(
      let* maybe_x = x in
      match maybe_x with
      | None   -> return None
      | Some x -> f x
    )

    (* let empty  = M.return (None) *)

    (* let mplus x y = M.(
      let* maybe_x = x in
      match maybe_x with
      | None   -> y
      | Some x -> Some x |> return
    ) *)
  
  end)

  let guard check =
    if check then
      Some () |> M.return
    else
      M.return None

end