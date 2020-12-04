module Day04


type PassportField =
    | BirthYear
    | IssueYear
    | ExpirationYear
    | Height
    | HairColor
    | EyeColor
    | PassportId
    | CountryId

module PassportField =
    let fromString str =
        match str with
            | "byr" -> BirthYear
            | "iyr" -> IssueYear
            | "eyr" -> ExpirationYear
            | "hgt" -> Height
            | "hcl" -> HairColor
            | "ecl" -> EyeColor
            | "pid" -> PassportId
            | "cid" -> CountryId
            | _ -> failwithf "Unable to identify text [%s] as a PassportField" str

type PassportData = Map<PassportField, string>

let (|IntegerBetween|_|) (min, max) (str:string) =
    match System.Int32.TryParse(str) with
        | true, i when i >= min && i <= max -> Some i
        | _, _ -> None

let (|RegexMatch|_|) regex str =
    let m = System.Text.RegularExpressions.Regex(regex).Match(str)
    if m.Success
    then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None


let parseLine currentData (text:string) =
    let current = currentData |> Option.defaultValue Map.empty

    let folder acc (item:string) =
        let split = item.Split(':')
        let k = split.[0] |> PassportField.fromString
        let v = split.[1]

        acc |> Map.add k v

    text.Split(" ") |> Array.fold folder current

let parseText (text:string) =
    let (|EmptyString|NonEmptyString|) (str:string) =
        match str with
            | s when System.String.IsNullOrWhiteSpace(s) -> EmptyString
            | _ -> NonEmptyString

    let splitLines (str:string) = str.Split(System.Environment.NewLine)

    let parseLines (lines:string seq) =
        let folder acc (line:string) =
            let (prev, current) = acc
            match line, current with
                | EmptyString, None -> prev, None
                | EmptyString, Some s -> List.append prev [s], None
                | NonEmptyString, None -> prev, Some (parseLine None line)
                | NonEmptyString, Some s -> prev, Some (parseLine (Some s) line)

        let previousItems, maybeFinalItem =
            lines
            |> Seq.fold folder (List.empty, None)

        match maybeFinalItem with
            | None -> previousItems
            | Some s -> List.append [s] previousItems

    text
    |> splitLines
    |> parseLines

module Validators =
    // Part 1 validator
    let containsEverythingButCid (p:Map<PassportField,_>) =
        // I like F#'s ability to write this without the parens
        p.ContainsKey BirthYear &&
            p.ContainsKey IssueYear  &&
            p.ContainsKey ExpirationYear &&
            p.ContainsKey Height &&
            p.ContainsKey HairColor &&
            p.ContainsKey EyeColor &&
            p.ContainsKey PassportId


    // Part 2 validators

    let private validateField field validator (p:PassportData) =
        p
        |> Map.tryFind field
        |> Option.map validator
        |> Option.defaultValue false

    let private isIntBetween (min,max) v =
        match v with
            | IntegerBetween (min, max) _ -> true
            | _ -> false

    let hasValidBirthYear =
        validateField BirthYear (isIntBetween (1920, 2002))

    let hasValidIssueYear =
        validateField IssueYear (isIntBetween (2010, 2020))

    let hasValidExpirationYear =
        validateField ExpirationYear (isIntBetween (2020, 2030))

    let hasValidHeight p =
        let validator height =
            match height with
                | RegexMatch @"(\d+)cm" [ cm ] ->
                    isIntBetween (150, 193) cm

                | RegexMatch @"(\d+)in" [ inches ] ->
                    isIntBetween (59, 76) inches

                | _ -> false

        validateField Height validator p

    let hasValidHairColor (p:PassportData) =
        let validator color =
            match color with
                | RegexMatch @"^#[0-9a-f]{6}$" _ -> true
                | _ -> false

        validateField HairColor validator p

    let hasValidEyeColor p =
        let validator s =
            match s with
                | "amb"
                | "blu"
                | "brn"
                | "gry"
                | "grn"
                | "hzl"
                | "oth"
                    -> true

                | _ -> false

        validateField EyeColor validator p

    let hasValidPassportId p =
        let validator s =
            match s with
                | RegexMatch @"^\d{9}$" _ -> true
                | _ -> false

        validateField PassportId validator p

    let performAllValidations p =
        hasValidBirthYear p &&
        hasValidIssueYear p &&
        hasValidExpirationYear p &&
        hasValidHeight p &&
        hasValidHairColor p &&
        hasValidEyeColor p &&
        hasValidPassportId p

