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
    |> List.length
    |> Assert.FailWith "%i"

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