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
    System.IO.File.ReadAllText("Samples/DayTwo.txt")
    |> parse
    |> punchIt 
    |> Assert.EqualTo 585
    
[<Fact>]
let ``Second Initial Test``() =
    [48; 52; 50; 51; 54] |> testy2 |> Assert.EqualTo (Increasing 54)
    [21; 23; 24; 27; 24] |> testy2 |> Assert.EqualTo (Increasing 27)    
    [66; 68; 64; 61; 58; 57; 54] |> testy2 |> Assert.EqualTo (Decreasing 54)
    [24; 25; 24; 23; 21; 19; 18; 17] |> testy2 |> Assert.EqualTo  (Decreasing 17)
    [88; 90; 88; 86; 84; 82; 80] |> testy2 |> Assert.EqualTo  (Decreasing 80)
    [3; 8; 6; 8; 10; 12; 15]|> testy2 |> Assert.EqualTo (Increasing 15)
    [7;6;4;2;1] |> test2 |> Assert.EqualTo (Decreasing 1)
    [1;2;7;8;9] |> test2 |> Assert.EqualTo Unsafe    
    [9;7;6;2;1] |> test2 |> Assert.EqualTo Unsafe
    [1;3;2;4;5] |> test2 |> Assert.EqualTo (Increasing 5)
    [8;6;4;4;1] |> test2 |> Assert.EqualTo (Decreasing 1)
    [1;3;6;7;9] |> test2 |> Assert.EqualTo (Increasing 9)    
    parse input
    |> punchIt2
    |> Assert.EqualTo 4
[<Fact>]
let ``Second Major Test``() =
    System.IO.File.ReadAllText("Samples/DayTwo.txt")
    |> parse 
    |> List.map testy2
    |> List.sumBy (function | Unsafe -> 0 | _ -> 1)    
    |> Assert.EqualTo 626

    