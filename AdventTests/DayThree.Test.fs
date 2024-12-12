module DayThree.Test
open Xunit
open MyAssert

let sample =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

[<Theory>]
[<InlineData("mul(2,4)", 8)>]
[<InlineData("mul(5,5)+mul(32,64]", 25)>]
[<InlineData("mul(11,8)", 88)>]
[<InlineData("mul(32,64]then", 0)>]
[<InlineData("mul(3,7]", 0)>]
let ``Samples``(input,expected) =
    let result = mul input
    result |> Assert.EqualTo expected
[<Fact>]
let ``Day Three - Part One - Sample``() =
    let result = mul sample
    result |> Assert.EqualTo 161

[<Fact>]
let ``Day Three - Part One - Input``() =
    let input = System.IO.File.ReadAllText "Samples/DayThree.txt"
    let result = mul input
    result |> Assert.EqualTo 166630675

[<Theory>]
[<InlineData("mul(2,4)", 8)>]
[<InlineData("mul(5,5)+mul(32,64]", 25)>]
[<InlineData("mul(11,8)", 88)>]
[<InlineData("mul(32,64]then", 0)>]
[<InlineData("mul(3,7]", 0)>]
[<InlineData("do()mul(3,7]", 0)>]

let ``Samples With Do``(input,expected) =
    let i = 0    
    match input with
    | Mul (_,a)  -> Assert.EqualTo expected a
    | Next  -> Assert.True true
    | Do i -> Assert.EqualTo 4 i
    | _ -> Assert.Fail "whoops"

let sample2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
[<Fact>]
let ``Single Input``() =    
    match sample2.Substring 1 with
    | Mul (i,a)  ->
        Assert.EqualTo 8 a        
    | other -> Assert.FailWith "%A" other
    dontRegex.Match(sample2.Substring 20)
             .Success |> Assert.True
    
[<Fact>]
let ``Day Three - Part Two - Sample``() =
    let result = mul2 sample2
    result |> Assert.EqualTo 48
[<Fact>]
let ``Day Three - Part Two - Input``() =
    let input = System.IO.File.ReadAllText "Samples/DayThree.txt"
    let result = mul2 input
    result |> Assert.FailWith "%i"

