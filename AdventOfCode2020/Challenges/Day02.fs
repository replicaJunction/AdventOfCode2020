module Day02

let countChars (str:string) =
    let folder acc item =
        let newCount =
            match acc |> Map.tryFind item with
                | Some s -> s + 1
                | None -> 1

        acc |> Map.add item newCount

    str.ToCharArray()
    |> Array.fold folder Map.empty

type Entry = {
    X: int
    Y: int
    Character: char
    Password: string // Why is a password exposed in a plain string?
}

module Entry =
    open System.Text.RegularExpressions

    [<Literal>]
    let private groupName_x = "x"

    [<Literal>]
    let private groupName_y = "y"

    [<Literal>]
    let private groupName_char = "char"

    [<Literal>]
    let private groupName_text = "text"

    [<Literal>]
    let private regex = @"(?<x>\d+)-(?<y>\d+)\s+(?<char>\w):\s+(?<text>\w+)"

    let tryParse (str:string) =
        let rmatch = Regex.Match(str, regex)
        match rmatch.Success with
            | false -> None
            | true ->
                let min = rmatch.Groups.[groupName_x].Value |> int
                let max = rmatch.Groups.[groupName_y].Value |> int
                let ch = rmatch.Groups.[groupName_char].Value |> char
                let password = rmatch.Groups.[groupName_text].Value
                Some {
                    X = min
                    Y = max
                    Character = ch
                    Password = password
                }

    let isValidPartOne (entry:Entry) =
        // Part 1 interprets X and Y as a minimum and maximum
        countChars entry.Password
        |> Map.tryFind entry.Character
        |> Option.map (fun count ->
            count >= entry.X &&
            count <= entry.Y)
        |> Option.defaultValue false

    let isValidPartTwo (entry:Entry) =
        // Part 2 interprets X and Y as two positions in the string (1-based, not 0-based!).
        // Exactly one of these must contain the provided character.

        let c1 = entry.Password.[entry.X - 1]
        let c2 = entry.Password.[entry.Y - 1]

        (c1 = entry.Character || c2 = entry.Character) && not (c1 = entry.Character && c2 = entry.Character)

let countValid validator (lines:string seq) =
    lines
    |> Seq.map (
        Entry.tryParse
        >> Option.map validator
        >> Option.defaultValue false
        )
    |> Seq.groupBy id
    |> Seq.map (fun (k, items) -> (k, Seq.length items))
    |> Seq.toList

let countValidFromFile validator path =
    System.IO.File.ReadLines(path)
    |> countValid validator


