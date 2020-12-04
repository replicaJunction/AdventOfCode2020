namespace Challenges.Day04

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit

open Day04

type SmallInt = SmallInt of int with
    static member op_Explicit (SmallInt i) = i

type Digit = Digit of int with
    static member op_Explicit (Digit d) = d

type ArbitraryModifiers =
    static member SmallInt() =
        Arb.from<int>
        |> Arb.filter (fun i -> i > 0 && i < 256)
        |> Arb.convert SmallInt int

    static member Digit() =
        let mapper i =
            let firstDigit i =
                (string i).Substring(0, 1) |> int

            match i with
                | 0 -> 0
                | x when x > 0 && x <= 9 -> x
                | x when x > 9 -> firstDigit x
                | x when x < 0 && x > -10 -> System.Math.Abs(x)
                | _ -> i |> System.Math.Abs |> firstDigit

        Arb.from<int>
        |> Arb.mapFilter mapper (fun _ -> true)
        |> Arb.convert Digit int

type ModPropertyAttribute () =
    inherit PropertyAttribute(Arbitrary = [|
        typeof<ArbitraryModifiers>
    |])

module Part1Examples =

    [<Literal>]
    let example = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

    [<Fact>]
    let ``Parsing the first line of the example provides the four expected keys`` () =
        let line =
            example.Split(System.Environment.NewLine)
            |> Array.head

        // Maps are unordered. Be sure to sort the result data.

        let expected = [
            (ExpirationYear, "2020")
            (EyeColor, "gry")
            (HairColor, "#fffffd")
            (PassportId, "860033327")
        ]

        let result = parseLine None line

        result
        |> Map.toList
        |> List.sortBy (fun (k,v) -> sprintf "%A" k)
        |> should equal expected

    [<Fact>]
    let ``Four sets of passport data are parsed from the example`` () =
        example
        |> parseText
        |> Seq.length
        |> should equal 4

    [<Fact>]
    let ``Example contains 2 valid passports according to Part 1`` () =
        example
        |> parseText
        |> List.filter Validators.containsEverythingButCid
        |> List.length
        |> should equal 2

module Part2Examples =
    [<Literal>]
    let invalidExamples = """
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"""

    [<Literal>]
    let validExamples = """
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"""

    [<Fact>]
    let ``All of the provided invalid examples are invalid`` () =
        invalidExamples
        |> parseText
        |> List.forall (fun item ->
            Validators.performAllValidations item = false
            )
        |> should be True

    [<Fact>]
    let ``All of the provided valid examples are valid`` () =
        validExamples
        |> parseText
        |> List.forall Validators.performAllValidations
        |> should be True


module Validators =
    open Validators

    [<Property>]
    let ``hasValidBirthYear: only values between 1920 and 2002 are considered valid``
        (year:uint) =

        let expected = year >= 1920u && year <= 2002u

        let p =
            Map.empty
            |> Map.add BirthYear (string year)

        hasValidBirthYear p
        |> should equal expected

    [<Property>]
    let ``hasValidBirthYear: returns false if BirthYear is not a valid int``
        (field:PassportField)
        (value:string) =

        // Just in case the value is actually a valid number,
        // fudge it a bit
        let value' = value + "foobar"

        let p =
            Map.empty
            |> Map.add field value'

        hasValidBirthYear p
        |> should be False


    [<ModProperty>]
    let ``hasValidHeight: a value in inches must be in the range 59-76``
        (inches:SmallInt) =

        let expected =
            let i = int inches
            i >= 59 && i <= 76

        let str = sprintf "%iin" (int inches)
        let p =
            Map.empty
            |> Map.add Height str

        hasValidHeight p
        |> should equal expected

    [<ModProperty>]
    let ``hasValidHeight: a value in cm must be in the range 150-193``
        (cm:SmallInt) =

        let expected =
            let i = int cm
            i >= 150 && i <= 193

        let str = sprintf "%icm" (int cm)
        let p =
            Map.empty
            |> Map.add Height str

        hasValidHeight p
        |> should equal expected

