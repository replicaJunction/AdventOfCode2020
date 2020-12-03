namespace Challenges

module Day02 =

    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open FsUnit.Xunit

    open Day02

    [<Property>]
    let ``countChars returns a map with the correct number of keys``
        (items': Map<char, PositiveInt>) =

        // Remove the PositiveInt decorator
        let items =
            items'
            |> Map.map (fun c (PositiveInt i) -> i)

        // Create a test string with each character, c,
        // repeated i times
        let text =
            items
            |> Map.toSeq
            |> Seq.map (fun (c,i) -> String.replicate i (string c))
            |> String.concat ""

        let result = countChars text

        result
        |> Map.count
        |> should equal (items |> Map.count)

    [<Property>]
    let ``countChars returns the correct count of each character``
        (items': Map<char, PositiveInt>) =

        let items =
            items'
            |> Map.map (fun c (PositiveInt i) -> i)
            |> Map.toSeq

        let text =
            items
            |> Seq.map (fun (c,i) -> String.replicate i (string c))
            |> String.concat ""

        let result = countChars text

        result
        |> Map.forall (fun k v ->
            items
            |> Seq.find (fun (c,_) -> k = c)
            |> snd
            |> (=) v)
        |> should be True

    [<Theory>]
    [<InlineData(@"1-3 a: abcde", 1, 3, 'a', "abcde")>]
    [<InlineData(@"1-3 b: cdefg", 1, 3, 'b', "cdefg")>]
    [<InlineData(@"2-9 c: ccccccccc", 2, 9, 'c', "ccccccccc")>]

    // A couple from my puzzle input that I added for extra validation
    [<InlineData(@"7-16 m: vxjgnbmmbzsxlhblj", 7, 16, 'm', "vxjgnbmmbzsxlhblj")>]
    [<InlineData(@"13-16 v: vvvvlvvvvvvxwvvv", 13, 16, 'v', "vvvvlvvvvvvxwvvv")>]
    let ``Entry - Examples in the problem are parsed as Entry records correctly``
        (rawText:string)
        (min:int)
        (max:int)
        (ch:char)
        (pw:string) =

        let expected = Some {
            X = min
            Y = max
            Character = ch
            Password = pw
        }

        Entry.tryParse rawText |> should equal expected

    [<Property>]
    let ``Entry - tryParse returns None if the text does not match``
        (text':NonNull<string>) =

        let (NonNull text) = text'

        let text = text.Replace(":", "")
        Entry.tryParse text |> should equal None

    [<Property>]
    let ``Entry - isValidPartOne returns true iif the character is within the expected bounds``
        (x:PositiveInt)
        (y:PositiveInt)
        (count:PositiveInt)
        (c:char)
        (noise: NonWhiteSpaceString) =

        let min, max = if x < y then x, y else y, x
        let text =
            let charRepeated =
                c
                |> string
                |> String.replicate (int count)

            let noise = (string noise).Replace(string c, "")

            noise + charRepeated

        let entry = {
            X = int min
            Y = int max
            Character = c
            Password = text
        }

        let expected = count >= min && count <= max

        Entry.isValidPartOne entry
        |> should equal expected

    [<Property>]
    let ``isValidPartTwo returns false if both positions contain the provided character``
        (x:byte)
        (y:byte)
        (c:char) =

        let x' = int x
        let y' = int y
        let c' = string c

        let text = c' |> String.replicate 300
            //if x' > y' then
            //    String.replicate (x' + 1) c'
            //else
            //    String.replicate (y' + 1) c'

        let entry = {
            X = x'
            Y = y'
            Character = c
            Password = text
        }

        Entry.isValidPartTwo entry
        |> should be False

    [<Property>]
    let ``isValidPartTwo returns false if neither position contains the provided character``
        (x:byte)
        (y:byte)
        (c:char) =

        let x' = int x
        let y' = int y
        let c' = c |> int |> (+) 1 |> char |> string

        let text = c' |> String.replicate 300

        let entry = {
            X = x'
            Y = y'
            Character = c
            Password = text
        }

        Entry.isValidPartTwo entry
        |> should be False

    [<Property>]
    let ``isValidPartTwo returns true if one position contains the character and the other does not``
        (x:byte)
        (y:byte)
        (c:char)
        (useSecondPosition:bool) =

        let x' = int x
        let y' = int y
        let c' = c |> int |> (+) 1 |> char |> string

        let text =
            let t = c' |> String.replicate 300

            if useSecondPosition then
                t.Insert(y' - 1, string c)
            else
                t.Insert(x' - 1, string c)

        let entry = {
            X = x'
            Y = y'
            Character = c
            Password = text
        }

        Entry.isValidPartTwo entry
        |> should be True

