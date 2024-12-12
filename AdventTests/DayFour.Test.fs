module DayFour.Test
open System.IO
open Xunit
open MyAssert
open System
open Advent

let testCase =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """
    |> ofString
let case ()=
    File.ReadAllText "Samples/DayFour.txt"
    |> ofString
[<Fact>]
let ``try Test``() =
    [ for i in 0..9 do
        for j in 0..9 do
            yield locus (i,j) testCase ] 
    |> List.sum
    |> Assert.EqualTo 18
    
    
[<Fact>]
let ``try Real`` () =
    let input = case()
    let m = input.GetLength 0
    let n = input.GetLength 1
    [ for i in 0..m-1 do
        for j in 0..n-1 do
            yield locus (i,j) input ] 
    |> List.sum
    |> Assert.EqualTo 2524
[<Fact>]
let ``Try Test Part 2``() =
    let input = testCase
    [ for i in 0..9 do
        for j in 0..9 do
            yield xmasCounterMain (i,j) input]
    |> List.sum
    |> Assert.EqualTo 9

[<Fact>]
let ``try Real Part Deux`` () =
    let input = case()
    let m = input.GetLength 0
    let n = input.GetLength 1
    [ for i in 0..m-1 do
        for j in 0..n-1 do
            yield xmasCounterMain (i,j) input ] 
    |> List.sum
    |> Assert.EqualTo 1873