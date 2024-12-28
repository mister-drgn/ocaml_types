module type MonoFunctor = sig
  type c
  type elem

  val map: (elem -> elem) -> c -> c
end

module type MonoMonad = sig
  include MonoFunctor

  val return: elem -> c

  val bind: c -> (elem -> c) -> c
end

module type MonoMonadPlus = sig
  include MonoMonad

  val empty: c

  val mplus: c -> c -> c
end

module DoMonoFunctor(F: MonoFunctor) = struct
  include F

  let ( let+ ) m f = map f m

  let ( <$> ) = map

  let ( <$ ) value = map (Fun.const value) 
end

module DoMonoMonad(M: MonoMonad) = struct
  include DoMonoFunctor(M)
  include M

  let ( >>= ) = bind

  let ( let* ) = bind

end

module DoMonoMonadPlus(M: MonoMonadPlus) = struct
  include DoMonoMonad(M)
  include M

  let mconcat xs = 
    List.fold_left M.mplus M.empty xs

  let guard check result =
    if check then
      M.return result
    else
      M.empty
end