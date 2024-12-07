module DayOne.Test
open DayOne
open System
open Xunit
open Advent
open MyAssert
let input =
    """
        3   4
        4   3
        2   5
        1   3
        3   9
        3   3
    """
[<Fact>]
let ``Initial Test``() =
    punchIt input
    |> Assert.EqualTo 11

[<Fact>]
let ``Second Phase Test``() =
    punchIt2 input
    |> Assert.EqualTo 31
    
[<Fact>]
let ``Second Test``() =
    System.IO.File.ReadAllText("DayOne.txt")
    |> punchIt 
    |> Assert.EqualTo 2086478

[<Fact>]
let ``Second Phase Test 2``() =
    System.IO.File.ReadAllText("DayOne.txt")
    |> punchIt2
    |> Assert.FailWith "%i"
    



