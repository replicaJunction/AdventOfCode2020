namespace Challenges

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit

open Day03

type NonHashChar = NonHashChar of char
type ValidChar = ValidChar of char
type SmallInt = SmallInt of int with
    static member op_Explicit (SmallInt i) = i

// Helper type to track both a Row and the length of its
// seed sequence
type RowAndLength = RowAndLength of Row * int

module private Gens =
    let rowGen length = gen {
        let! chars =
            Gen.elements ["#"; "."]
            |> Gen.listOfLength length

        return
            chars
                |> String.concat ""
                |> Row.fromString
    }

    let rowAndLengthGen = gen {
        let! len = Gen.choose (4, 24)
        let! row = rowGen len
        return RowAndLength (row, len)
    }

    let areaGen : Gen<Area> = gen {
        let! rowLength = Gen.choose (4, 24)
        let! areaLength = Gen.choose (8, 30)

        return!
            rowGen rowLength
            |> Gen.listOfLength areaLength
    }

    let vectorGen : Gen<Vector> = gen {
        let! x = Gen.choose (1, 3)
        let! y = Gen.choose (1, 3)

        return { X = x; Y = y }
    }

type ArbitraryModifiers =
    static member NonHashChar () =
        Arb.from<char>
        |> Arb.filter (fun c -> c <> '#')
        |> Arb.convert NonHashChar (fun (NonHashChar c) -> c)

    static member ValidChar () =
        Gen.elements ['#'; '.']
        |> Arb.fromGen
        |> Arb.convert ValidChar (fun (ValidChar c) -> c)

    static member SmallInt () =
        Arb.from<int>
        |> Arb.mapFilter (fun i ->
            match i with
                | x when x < 1 -> 1
                | x when x > 10 -> 10
                | _ -> i
            ) (fun _ -> true)
        |> Arb.convert SmallInt int

    static member RowAndLength () =
        Arb.fromGen Gens.rowAndLengthGen

    static member Area () =
        Arb.fromGen Gens.areaGen

    static member Vector() = Gens.vectorGen |> Arb.fromGen

type ModPropertyAttribute () =
    inherit PropertyAttribute(Arbitrary = [|
        typeof<ArbitraryModifiers>
    |])

module Day03 =
    [<Fact>]
    let ``Space - The # character maps to Space.Tree`` () =
        Space.fromChar '#'
        |> should equal Space.Tree

    [<ModProperty>]
    let ``Space - Any character other than # maps to Space.Empty``
        (c':NonHashChar) =

        let (NonHashChar c) = c'

        Space.fromChar c
        |> should equal Space.Empty

    [<ModProperty>]
    let ``Row - fromString matches Space.fromChar``
        (chars: ValidChar list) =

        let str =
            chars
            |> List.map (fun (ValidChar c) -> string c)
            |> String.concat ""

        let result =
            Row.fromString str
            |> Seq.take chars.Length
            |> Seq.toList

        chars
        |> List.mapi (fun i (ValidChar c) ->
            result.[i] = Space.fromChar c
            )
        |> List.forall id
        |> should be True

    [<ModProperty>]
    let ``Row - Item at index i matches item at index (i + length)``
        (i:byte)
        (rowLen:RowAndLength) =

        let (RowAndLength (row, len)) = rowLen

        let x = int i
        let y = x + len

        let items = row |> Seq.take (y + 1) |> Seq.toList

        items.[x]
        |> should equal items.[y]

    [<Literal>]
    let example = """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""

    [<Fact>]
    let ``Area - createPath from the example returns the expected path`` () =
        let area = Area.fromString example
        let path = Area.createPath {X=3;Y=1} area |> Seq.toList

        let expected = [
            {X=0;Y=0}
            {X=3;Y=1}
            {X=6;Y=2}
            {X=9;Y=3}
            {X=12;Y=4}
            {X=15;Y=5}
            {X=18;Y=6}
            {X=21;Y=7}
            {X=24;Y=8}
            {X=27;Y=9}
            {X=30;Y=10}
        ]

        path
        |> should equal expected

    [<ModProperty>]
    let ``Area - createPath will always return a path of length (number of rows / y slope) (rounded up)``
        (slope:Vector)
        (area:Area) =

        let expected = (float (List.length area)) / (float slope.Y) |> System.Math.Ceiling |> int

        let path = Area.createPath slope area |> Seq.toList

        path.Length
        |> should equal expected

    [<Fact>]
    let ``Area - treesOnPath works for the provided example`` () =
        let area = Area.fromString example

        Area.treesOnPath {X=3;Y=1} area
        |> should equal 7

    [<Fact>]
    let ``Area - productOfTreesOnPaths provides the correct answer for the provided example`` () =
        let area = Area.fromString example

        let paths = [
            {X=1;Y=1}
            {X=3;Y=1}
            {X=5;Y=1}
            {X=7;Y=1}
            {X=1;Y=2}
        ]

        Area.productOfTreesOnPaths paths area
        |> should equal 336

