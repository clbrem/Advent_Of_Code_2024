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
        |> Assert.EqualTo 4122618559853L
    
    
    
[<Theory>]
[<InlineData(298L, 3L, 1000L)>]
[<InlineData(100024L, 6L, 1000000L)>]
[<InlineData(100000000000000001L, 18L,1000000000000000000L )>]
[<InlineData(900000000120000456L, 18L, 1000000000000000000L)>]
let ``digits``(input: int64, n: int64, modulus: int64) =
    Int64.digits input |> Assert.EqualTo n
    Int64.modulus input |> Assert.EqualTo modulus
//     
// [<Fact>]
// let ``WTF`` ()=
    
    
[<Theory>]
[<InlineData("190:10 19", true)>]
[<InlineData("3267:81 40 27", true)>]
[<InlineData("83:17 5", false)>]
[<InlineData("156: 15 6", true)>]
[<InlineData("7290: 6 8 6 15", true)>]
[<InlineData("161011: 16 10 13", false)>]
[<InlineData("192: 17 8 14", true)>]
[<InlineData("21037: 9 7 18 13", false)>]
[<InlineData("292: 11 6 16 20", true)>]
let ``Can Solve With Concat``(eqn, expected) =
    Equation.ofString eqn     
    |> Equation.canSolveWithConcat
    |> Assert.EqualTo expected
[<Fact>]    
let ``Can Solve Actual With Concat`` () =    
        File.ReadAllText "Samples/DaySeven.txt"
        |> String.lines    
        |> List.map Equation.ofString
        |> List.filter Equation.canSolveWithConcat
        |> List.sumBy Equation.testValue
        |> Assert.FailWith "%i"