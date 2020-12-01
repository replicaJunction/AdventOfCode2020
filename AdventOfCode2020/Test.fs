module Test

let single f input expected =
    let f' () = f input
    try
        let actual = f' |> Lib.timed
        printfn ""

        if actual = expected then
            printfn "OK - %A" actual
            true
        else
            printfn "Error - expected %A but received %A" expected input
            false

    with e ->
        printfn "Error - exception thrown:\n%A" e
        false

let multi (f : 'input -> 'output) (inputsAndExpectedOutputs : ('input * 'output) list) =
    let rec helper remainingItems (input, expected) =
        let f' () = f input
        let actual = Lib.timed f'
        let result = expected = actual

        match result, remainingItems with
            | false, _ ->
                printfn "FAILED - for input %A, expected %A but got %A" input expected actual
                false

            | true, [] ->
                printfn "OK - for input %A, received the correct value %A" input actual
                printfn "End of testing"
                true
            | true, x::xs ->
                printfn "OK - for input %A, received the correct value %A" input actual
                let (newInput, newExpected) = x
                helper xs (newInput, newExpected)

    let firstInput, firstExpected = List.head inputsAndExpectedOutputs
    helper (List.skip 1 inputsAndExpectedOutputs) (firstInput, firstExpected)

let eval f input =
    let f' () = f input
    let result = f' |> Lib.timed
    printfn "\n\nPuzzle result: %A" result

