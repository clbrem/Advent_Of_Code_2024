module DayFive.Test
open Xunit
open MyAssert
open Advent
let inputRules =
    """     47|53
            97|13
            97|61
            97|47
            75|29
            61|13
            75|53
            29|13
            97|29
            53|29
            61|53
            97|53
            61|29
            47|13
            75|47
            97|75
            47|61
            75|61
            47|29
            75|13
            53|13
    """ |> Rules.ofString
let input =
    """75,47,61,53,29
       97,61,53,29,13
       75,29,13
       75,97,47,61,53
       61,13,29
       97,13,75,29,47
    """ |> String.lines |> List.map Sequence.ofString
[<Fact>]
let ``Can Find Middle``() =
    let a = [1;2;3;4;5]
    Sequence.middle a |> Assert.EqualTo 3

[<Fact>]
let ``Can Check Sequence``() =
    let rules = inputRules 
    Rules.checkSequence rules [75;47;61;53;29 ] |> Assert.Some (Assert.EqualTo 61) 
    Rules.checkSequence rules [97;61;53;29;13 ] |> Assert.Some (Assert.EqualTo 53)
    Rules.checkSequence rules [75;29;13] |> Assert.Some (Assert.EqualTo 29)
    Rules.checkSequence rules [75;97;47;61;53] |> Assert.IsNone
    Rules.checkSequence rules [61;13;29] |> Assert.IsNone
    Rules.checkSequence rules [97;13;75;29;47] |> Assert.IsNone
[<Fact>]
let ``Manage Test Input``() =
    let rules = inputRules 
    let sequences = input 
    sequences
    |> List.map (Rules.checkSequence rules)
    |> List.sumBy (function Some x -> x | None -> 0)
    |> Assert.EqualTo 143
[<Fact>]
let ``Manage Input``() =
    let rules = System.IO.File.ReadAllText "Samples/DayFiveRules.txt" |> Rules.ofString
    let sequences = System.IO.File.ReadAllText "Samples/DayFive.txt" |> String.lines |> List.map Sequence.ofString
    sequences
    |> List.map (Rules.checkSequence rules)
    |> List.sumBy (function Some x -> x | None -> 0)
    |> Assert.EqualTo 4814

[<Fact>]
let ``Re Sort``() =
    Rules.reSort inputRules [75;47;61;53;29 ]
    |> Assert.EqualTo [75;47;61;53;29]
    Rules.reSort inputRules [97;61;53;29;13 ]
    |> Assert.EqualTo [97;61;53;29;13 ]
    Rules.reSort inputRules [75;29;13]
    |> Assert.EqualTo [75;29;13]
    Rules.reSort inputRules [75;97;47;61;53]
    |> Assert.EqualTo [97;75;47;61;53]
    Rules.reSort inputRules [61;13;29]
    |> Assert.EqualTo [61;29;13]
    Rules.reSort inputRules [97;13;75;29;47]
    |> Assert.EqualTo [97;75;47;29;13]
[<Fact>]
let ``Can Test Second Half``() =
    let rules = inputRules
    let sequences = input
    sequences
    |> List.map (Rules.resortSequence rules)
    |> List.sumBy (function Some x -> x | None -> 0)
    |> Assert.EqualTo 123
[<Fact>]
let ``Can Run Second Half``() =
    let rules = System.IO.File.ReadAllText "Samples/DayFiveRules.txt" |> Rules.ofString
    let sequences = System.IO.File.ReadAllText "Samples/DayFive.txt" |> String.lines |> List.map Sequence.ofString    
    sequences
    |> List.map (Rules.resortSequence rules)
    |> List.sumBy (function Some x -> x | None -> 0)
    |> Assert.EqualTo 123    