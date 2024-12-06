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

    
    
    



