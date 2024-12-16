module DayEight.Test
open Xunit
open MyAssert
open Advent
open System.IO

let answer =
    """
    ......#....#
    ...#....0...
    ....#0....#.
    ..#....0....
    ....0....#..
    .#....A.....
    ...#........
    #......#....
    ........A...
    .........A..
    ..........#.
    ..........#.

"""

let sample =
    """
    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............
    
    """

[<Fact>]
let ``Can Calculate Antinode``() =    
    Grid.antiNode (12,12) ((2,5), (1,8))
        |> Assert.EqualTo (set [(0, 11); (3,2)])

[<Fact>]
let ``Can Get Antinodes``() =    
    Grid.ofString sample
    |> Grid.antiNodes
    |> Assert.EqualTo
           (set [
        (0,6);(0,11);(1,3);(2,4);(2,10);(3,2);(4,9)
        (5,1);(5,6);(6,3);(7,0);(7,7);(10,10);(11,10)
    ])

[<Fact>]
let ``Part A``() =
    File.ReadAllText "Samples/DayEight.txt"
    |> Grid.ofString 
    |> Grid.antiNodes
    |> Set.count
    |> Assert.EqualTo 341
    
[<Fact>]
let ``What Are We Getting Into``() =
    let count n =
        n * (n - 1) / 2
    File.ReadAllText "Samples/DayEight.txt"
    |> Grid.ofString
    |> Grid.antennae
    |> Map.values
    |> List.ofSeq
    |> List.sumBy (List.length >> count)
    |> Assert.EqualTo 324
    

[<Fact>]
let ``Is Collinear``() =    
    let items = Grid.antiNode (12,12) ((2,5), (1,8))
    Set.iter (Grid.collinear (2,5) (1,8) >> Assert.True) items
    set [(0,6);(0,11);(1,3);(2,4);(2,10);(3,2);(4,9);
         (5,1);(5,6);(6,3);(7,0);(7,7);(10,10);(11,10)]
    |> Set.difference
    <| items
    |> Set.iter (Grid.collinear (2,5) (1,8) >> Assert.False)

[<Fact>]
let ``Part B with test``() =
    let arr =
        sample
            |> String.lines
            |> List.map String.trim
            |> List.map String.chars
            |> Array2D.ofList
    let grid = Grid.ofArray arr
    let linSpace = Grid.linSpace grid
    Array2D.fold (fun acc (i,j) _ -> if Grid.test linSpace (i,j) then acc + 1 else acc) 0 arr
    |> Assert.EqualTo 34

[<Fact>]
let ``Part B with data``() =
    let arr =
        File.ReadAllText "Samples/DayEight.txt"
            |> String.lines
            |> List.map String.trim
            |> List.map String.chars
            |> Array2D.ofList
    let grid = Grid.ofArray arr
    let linSpace = Grid.linSpace grid
    Array2D.fold (fun acc (i,j) _ -> if Grid.test linSpace (i,j) then acc + 1 else acc) 0 arr
    |> Assert.EqualTo 34