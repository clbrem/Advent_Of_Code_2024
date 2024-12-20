module DayTen.Test
open Advent
open MyAssert
open Xunit
open System.IO
let sample =
    """
    0123
    1234
    8765
    9876
    """ |> TopoMap.ofString
let bigSample =
    """
    89010123
    78121874
    87430965
    96549874
    45678903
    32019012
    01329801
    10456732
    """ |> TopoMap.ofString
let ratingSample =
    """
    012345
    123456
    234567
    345678
    406789
    567890
"""
[<Fact>]
let ``Can Find A Peak``() =
    let peaks = TopoMap.peaks2 sample 
    Assert.EqualTo 1 peaks    
    
    let peaky = TopoMap.peaks2  bigSample 
    Assert.EqualTo 36 peaky
[<Fact>]
let ``For Real THis Time``() =
    File.ReadAllText "Samples/DayTen.txt"
    |> TopoMap.ofString
    |> TopoMap.peaks2
    |> Assert.EqualTo  501
    
[<Fact>]
let ``Can Find Ratings``() =
    File.ReadAllText "Samples/DayTen.txt"
    |> TopoMap.ofString
    |> TopoMap.ratings
    |> Assert.EqualTo  81
    
    

