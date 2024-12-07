module DayTwo.Test
open Xunit
open MyAssert
open DayTwo

let input =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """
[<Fact>]
let ``Line by Line``() =
    test [9;7;6;2;1]
    |> Assert.EqualTo Unsafe
[<Fact>]
let ``Initial Test``() =
    parse input
    |> punchIt 
    |> Assert.EqualTo 2 
[<Fact>]
let ``Actual Test``() =
    System.IO.File.ReadAllText("DayTwo.txt")
    |> parse
    |> punchIt 
    |> Assert.FailWith "%i"