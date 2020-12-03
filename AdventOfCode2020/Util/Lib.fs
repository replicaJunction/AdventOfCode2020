module Lib

let timed f =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let result = f()
    stopwatch.Stop()

    printfn "(Elapsed time: %A)" stopwatch.Elapsed
    result

let factorial x =
    let rec f acc x =
        match x with
            | x when x < 1 -> 0
            | 1 -> acc
            | 2 -> acc * 2
            | _ -> acc * (f acc (x - 1))

    f 1 x

/// Generates all combinations of the given list
let combinations size set =
    // https://stackoverflow.com/a/4495708/4991083

    let rec helper acc size set = seq {
        match size, set with
        | n, x::xs ->
            if n > 0 then yield! helper (x::acc) (n - 1) xs
            if n >= 0 then yield! helper acc n xs
        | 0, [] -> yield acc
        | _, [] -> ()
    }

    helper List.empty size set

