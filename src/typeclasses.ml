module type Functor = sig
  type ('a, 'e) c

  val map: ('a -> 'b) -> ('a, 'e) c -> ('b, 'e) c
end

module type Applicative = sig
  include Functor
  val apply : (('a -> 'b), 'e) c -> ('a, 'e) c -> ('b, 'e) c

  val return: 'a -> ('a, 'e) c
end

module type Monad = sig
  include Applicative

  val return: 'a -> ('a, 'e) c

  val bind: ('a, 'e) c -> ('a -> ('b, 'e) c) -> ('b, 'e) c
end

module type MonadPlus = sig
  include Monad

  val empty: ('a, 'e) c
  
  val mplus: ('a, 'e) c -> ('a, 'e) c -> ('a, 'e) c
end

module DoFunctor(F: Functor) = struct
  include F

  let ( let+ ) m f = map f m

  let ( <$> ) = map

  let ( <$ ) value = map (Fun.const value)
end

module DoApplicative(A: Applicative) = struct
  include DoFunctor(A)
  include A

  let ( <*> ) f x = apply f x

  let product x y = return (fun x y -> (x, y)) <*> x <*> y

  let ( and+ ) = product
end

module DoMonad(M: Monad) = struct
  include DoApplicative(M)
  include M

  let ( >>= ) = bind

  let ( let* ) = bind

  let ( and* ) = ( and+ )

  let rec sequence = function
    | [] -> return []
    | hd :: tl ->
      let* x = hd in
      let* xs = sequence tl in
      return (x :: xs)

  let mapM f xs = sequence (List.map f xs)

  let forM xs f = sequence (List.map f xs)
end

module DoMonadPlus(M: MonadPlus) = struct
  include DoMonad(M)
  include M

  let mconcat xs = 
    List.fold_left M.mplus M.empty xs

  let guard check =
    if check then
      M.return ()
    else
      M.empty
end