module DayEleven.Test
open Advent
open MyAssert
open Xunit

let firstExample = "0 1 10 99 999"
let secondExample = "125 17"
let puzzleInput = "4329 385 0 1444386 600463 19 1 56615"

[<Fact>]
let ``Even Digits``() =
    match 99L with
    | EvenDigits (9L,9L) -> Assert.Pass
    | _ -> Assert.Fail "99 has even digits"
    match 100L with
    | EvenDigits _ -> Assert.Fail "100 has odd digits"
    | _ -> Assert.Pass
    match 2024L with
    | EvenDigits (20L,24L) -> Assert.Pass
    | _ -> Assert.Fail "2024 has even digits"
    match 1000L with
    | EvenDigits (10L,0L) -> Assert.Pass
    | _ -> Assert.Fail "2024 has even digits"
    
[<Fact>]
let ``Evaluate Some``() =
    firstExample |> String.split " " |> List.map String.toInt64
    |> evaluateMany 1
    |> Assert.EqualTo [1; 2024; 1; 0; 9; 9; 2021976]

[<Fact>]
let ``Evaluate More``() =
    secondExample |> String.split " " |> List.map String.toInt64
    |> evaluateMany 25
    |> List.length |> Assert.EqualTo 55312
    
[<Fact>]
let ``Evaluate Input``() =
    puzzleInput |> String.split " " |> List.map String.toInt64
    |> evaluateMany 25
    |> List.length |> Assert.EqualTo 55312    
