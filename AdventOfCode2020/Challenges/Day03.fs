module Day03

type Space =
    | Tree
    | Empty

module Space =
    let fromChar c =
        match c with
            | '#' -> Tree
            | _ -> Empty

    let toChar space =
        match space with
            | Tree -> '#'
            | Empty -> '.'

// A single row has an infinite number of columns
type Row = Space seq

// The whole area has a finite number of rows
type Area = Row list

module Row =
    let fromString (str:string) : Row =
        let items =
            str |>
            Seq.map Space.fromChar
            |> Seq.toArray

        Seq.initInfinite (fun i -> items.[i % items.Length])

    let toString len (row:Row) =
        row
        |> Seq.take len
        |> Seq.map (Space.toChar >> string)
        |> String.concat ""

module Area =
    let fromString (str:string) : Area =
        str
        |> Lib.String.splitLines
        |> List.map Row.fromString

    let at ((x:int), (y:int)) (area:Area) : Space =
        if y >= area.Length then failwithf "Could not read row %i from the provided area: length of the area is %i. Out of bounds." y area.Length

        area.[y]
        |> Seq.take (x+1)
        |> Seq.last

    let createPath ((xSlope:int),(ySlope:int)) (area:Area) =
        let rec nextPoint (x,y) = seq {
            yield (x,y)
            let x' = x + xSlope

            match y + ySlope with
                | y' when y' >= area.Length -> ()
                | y' ->
                    yield! (nextPoint (x', y'))
        }

        nextPoint (0,0)

    let spacesOnPath (xSlope, ySlope) (area:Area) =
        createPath (xSlope, ySlope) area
        |> Seq.map (fun p -> at p area)

    let treesOnPath (xSlope, ySlope) area =
        spacesOnPath (xSlope, ySlope) area
        |> Seq.countBy (fun space -> space = Space.Tree)
        |> Seq.find (fun (b,i) -> b)
        |> snd

    let productOfTreesOnPaths (paths: (int*int) list) area =
        // Tricky! Need to use a larger data type than int (which in F# is 32-bit signed).

        paths
        |> List.map (fun (x,y) -> treesOnPath (x,y) area |> uint64)
        |> List.fold (*) 1UL

