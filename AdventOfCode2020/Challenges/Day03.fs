module Day03

type Vector = {
    X: int
    Y: int
}

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
        |> Seq.cache

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

    let spaceAt (point:Vector) (area:Area) : Space =
        if point.Y >= area.Length then
            failwithf
                "Could not read row %i from the provided area: length of the area is %i. Out of bounds."
                point.Y
                area.Length

        area.[point.Y]
        |> Seq.skip point.X
        |> Seq.head

    let createPath (slope:Vector) (area:Area) =
        let rec nextPoint (point:Vector) = seq {
            yield point

            let next = {
                X = point.X + slope.X
                Y = point.Y + slope.Y
            }

            match next.Y with
                | y' when y' >= area.Length -> ()
                | _ -> yield! (nextPoint next)
        }

        nextPoint {X = 0; Y = 0}

    let spacesOnPath slope (area:Area) =
        createPath slope area
        |> Seq.map (fun p -> spaceAt p area)

    let treesOnPath slope area =
        spacesOnPath slope area
        |> Seq.filter (fun space -> space = Tree)
        |> Seq.length

    let productOfTreesOnPaths (paths: Vector list) area =
        // Tricky! Need to use a larger data type than int (which in F# is 32-bit signed).

        paths
        |> List.map (fun v -> treesOnPath v area |> uint64)
        |> List.fold (*) 1UL

