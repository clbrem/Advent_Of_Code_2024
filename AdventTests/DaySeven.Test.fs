module DaySeven.Test
open Xunit
open Advent
open MyAssert
open System.IO
open System
let sample =
    """
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    """
    |> String.lines
    |> List.map Equation.ofString

[<Theory>]
[<InlineData("190:10 19", true)>]
[<InlineData("3267:81 40 27", true)>]
[<InlineData("83:17 5", false)>]
[<InlineData("156: 15 6", false)>]
[<InlineData("7290: 6 8 6 15", false)>]
[<InlineData("161011: 16 10 13", false)>]
[<InlineData("192: 17 8 14", false)>]
[<InlineData("21037: 9 7 18 13", false)>]
[<InlineData("292: 11 6 16 20", true)>]

let ``Can Solve ``(eqn, expected) =
    Equation.ofString eqn     
    |> Equation.canSolve
    |> Assert.EqualTo expected
[<Fact>]    
let ``Can Solve Actual`` () =    
        File.ReadAllText "Samples/DaySeven.txt"
        |> String.lines    
        |> List.map Equation.ofString
        |> List.filter Equation.canSolve
        |> List.sumBy Equation.testValue
        |> Assert.FailWith "%i"
    
    
    
[<Theory>]
[<InlineData(298L, 3L)>]
[<InlineData(100024L, 6L)>]
[<InlineData(1000000000000000001L, 19L)>]
[<InlineData(9000000001200004560L, 19L)>]

let ``digits``(input: int64, n: int64) =
    let rec loop acc input =
        if input  = 0L then acc 
        else loop (acc + 1L) (input / 10L)
    loop 0 input |> Assert.EqualTo n
    