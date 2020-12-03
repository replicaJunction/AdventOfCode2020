#r @"bin\Debug\net5.0\AdventOfCode2020.dll"

System.IO.Path.Combine(
    __SOURCE_DIRECTORY__,
    "PuzzleInputs",
    "Day02.txt")
|> Day02.countValidFromFile Day02.Entry.isValidPartTwo
