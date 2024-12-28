
type ('a, 'e) t = 'e -> 'a * 'e

let get = fun state -> (state, state)

let put state = fun _ -> ((), state)

let modify f = fun state -> ((), f state)

let getState (_, state) = state

let getValue (value, _) = value

include Typeclasses.DoMonad(struct
  type ('a, 'e) c = 'e -> 'a * 'e

  let map f oldStateFn = fun state ->
    let a, final_state = oldStateFn state in
    let b = f a in
    (b, final_state)

  let apply f x = fun state ->
    let f', temp_state = f state in
    let x', final_state = x temp_state in
    (f' x', final_state)

  let return x = fun state -> (x, state)

  let bind oldStateFn f = fun state ->
    let a, temp_state = oldStateFn state in
    let b, final_state = f a temp_state in
    (b, final_state)

end)