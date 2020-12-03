#r @"bin\Debug\net5.0\AdventOfCode2020.dll"

let getPuzzleInput day =
    System.IO.Path.Combine(
        __SOURCE_DIRECTORY__,
        "PuzzleInputs",
        day
    )

// Day 2
//System.IO.Path.Combine(
//    __SOURCE_DIRECTORY__,
//    "PuzzleInputs",
//    "Day02.txt")
//|> Day02.countValidFromFile Day02.Entry.isValidPartTwo

//// Day 3, part 1
//System.IO.Path.Combine(
//    __SOURCE_DIRECTORY__,
//    "PuzzleInputs",
//    "Day03.txt")
//|> System.IO.File.ReadAllText
//|> Day03.Area.fromString
//|> Day03.Area.treesOnPath (3,1)
//|> printfn "Trees on path: %i"

// Day 3, part 2
let paths = [
    (1,1)
    (3,1)
    (5,1)
    (7,1)
    (1,2)
]

getPuzzleInput "Day03.txt"
|> System.IO.File.ReadAllText
|> Day03.Area.fromString
|> Day03.Area.productOfTreesOnPaths paths
|> printfn "Product of trees on the provided paths: %i"

