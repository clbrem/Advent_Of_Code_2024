module DaySix.Test
open Advent
open Xunit
open MyAssert
let testCase =
    """
    ....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...
    """
    |> Grid.ofString
    
[<Fact>]
let ``Can Find Guard``() =
    findGuard testCase
    |> Assert.Some (Assert.EqualTo { coord = (4,6); direction = North })

[<Fact>]
let ``Can Solve Part a``() =
    let guard = findGuard testCase
    traverse testCase guard.Value
    |> Set.count
    |> Assert.EqualTo 41
[<Fact>]
let ``Part a real``() =
    let input = System.IO.File.ReadAllText "Samples/DaySix.txt" |> Grid.ofString
    let guard = findGuard input
    traverse input guard.Value
    |> Set.count
    |> Assert.EqualTo 5269
[<Fact>]
let ``Part b test``() =
    let guard = findGuard testCase
    solve testCase guard.Value
    |> Assert.EqualTo 6
[<Fact>]
let ``Part b real``() =
    let input = System.IO.File.ReadAllText "Samples/DaySix.txt" |> Grid.ofString
    let guard = findGuard input
    solve input guard.Value
    |> Assert.EqualTo 6
    