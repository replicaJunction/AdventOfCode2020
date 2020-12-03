namespace LibTests

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

type OneToFive = OneToFive of int with
    static member op_Explicit(OneToFive o) = o

type OneToOneHundred = OneToOneHundred of int with
    static member op_Explicit(OneToOneHundred o) = o

type ArbitraryModifiers =
    static member OneToFive() =
        let filter (x:int) =
            x > 0 &&
            x < 6

        Arb.from<int>
        |> Arb.filter filter
        |> Arb.convert OneToFive int

    static member OneToOneHundred() =
        let filter (x:int) =
            x > 0 &&
            x < 100

        Arb.from<int>
        |> Arb.filter filter
        |> Arb.convert OneToOneHundred int

type ModPropertyAttribute () =
    // Here, we create a custom attribute that will save us a bit of
    // typing.
    //
    // Instead of constantly typing and re-typing this:
    // [<Property(Arbitrary = [| typeof<ArbitraryModifiers> |])>]
    // we will be able to type this:
    // [<ModProperty>]

    inherit PropertyAttribute(Arbitrary = [| typeof<ArbitraryModifiers> |])

//module ArbSanity =
//    [<Property>]
//    let ``OneToFive returns items in the range 1..5``
//        (x':OneToFive) =

//        let x = int x'

//        (x > 0 && x < 6)
//        |> should be True

//module Factorial =
//    [<Fact>]
//    let ``Factorial of 1 is 1`` () =
//        Lib.factorial 1 |> should equal 1

//    [<Property>]
//    let ``Test oracle`` (x':OneToFive) =
//        let x = int x'
//        let expected = [1..x] |> List.fold (*) 1

//        Lib.factorial x |> should equal expected

module Combinations =
    [<ModProperty>]
    let ``All generated lists are of the provided size``
        (sizeParam:OneToFive) =

        let size = int sizeParam
        let set = [1..size]

        let result = Lib.combinations size set

        result
        |> Seq.forall (fun x -> List.length x = size)
        |> should be True

    //[<ModProperty>]
    //let ``Correct number of results returned``
    //    (sizeParam:OneToFive)
    //    (numberOfItemsParam:OneToOneHundred) =

    //    let size = int sizeParam
    //    let numberOfItems = int numberOfItemsParam

    //    let set = [1..numberOfItems]
    //    let expected = System.Math.fa

    //    let result = Lib.combinations size set



    //    result
    //    |> Seq.length
    //    |> should be expected
