module Monads

let (>>=) x f = fun s0 -> let a, s1 = x s0 in f a s1

let returnS a = fun s -> a, s

type StateBuilder() =
    member m.Bind(x, f) = x >>= f
    member m.Return a = returnS a

let getState = fun s -> s, s

let setState s = fun _ -> (), s

let Execute m s = m s |> fst

let (>.>) a b = a >>= fun _ -> b

let NextShort state =
    let newState = (214013 * state) + 2531011
    let rand = int16 ((newState &&& System.Int32.MaxValue) >>> 16)
    (rand, newState)

let getRandom =
    getState >>=
        (fun s0 ->
            let x, s1 = NextShort s0
            in setState s1 >.> (returnS x))

let resultMonadic =
    let state = new StateBuilder()
    let m = state {
        let! a = getRandom
        let! b = getRandom
        let! c = getRandom
        return a + b + c
    }
    let seed = 0
    Execute m seed
