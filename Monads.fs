module Monads

type State<'a, 's> = State of ('s -> 'a * 's)

let runState (State s) a = s a
let getState = State (fun s -> (s,s))
let putState s = State (fun _ -> ((),s))

type StateBuilder() =
    member this.Return(a) = State (fun s -> (a,s))
    member this.Bind(m,k) =
        State (fun s ->
            let (a,s') = runState m s in runState (k a) s')
    member this.ReturnFrom (m) = m

let state = new StateBuilder()

let lift f =
    state {
        let! s = getState
        return! putState (f s)
    }

let NextShort state =
    let newState = (214013 * state) + 2531011
    let rand = int16 ((newState &&& System.Int32.MaxValue) >>> 16)
    (rand, newState)

let AggregateRandom (acc, s0) =
    let cur, s1 = NextShort s0 in (acc + cur, s1)

let resultMonadic =
    let AggregateRandom' = lift AggregateRandom
    let s = state {
        do! AggregateRandom'
        do! AggregateRandom'
        do! AggregateRandom'
    }
    let seed = 0
    runState s (int16 0, seed) |> snd |> fst
