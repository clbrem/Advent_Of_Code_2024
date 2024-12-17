module DayNine.Test
open Xunit
open MyAssert
open System
open System.IO
let sample = "2333133121414131402"

[<Fact>]
let ``Parse?``() =
    File.ReadAllText "Samples/DayNine.txt"
    |> Block.ofString
    |> fst
    |> List.length
    |> Assert.EqualTo 20000

[<Fact>]
let ``Can Calculate Checksum``() =
    [
        {id=0; start=0L; stop = 2L}
        {id=9; start=2L; stop = 4L}
        {id=8; start=4L; stop = 5L}
    ]
    |> List.map Block.File
    |> List.sumBy Block.checksum
    |> Assert.EqualTo (18L+27L+32L)
[<Fact>]
let ``test Part A``() =
    sample |> Block.ofString
    |> Memory.partA
    |> Assert.EqualTo 1928L
    
[<Fact>]
let ``Part A``() =
    File.ReadAllText "Samples/DayNine.txt"
    |> Block.ofString
    |> Memory.partA
    |> Assert.EqualTo 6463499258318L
[<Fact>]
let ``test Part B ``() =
    sample |> Block.ofString
    |> Memory.partB
    |> Assert.EqualTo 2858L