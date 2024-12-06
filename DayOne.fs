module DayOne
open Advent
open System
let punchIt =
    String.lines 
    >> List.map (String.words >> List.map String.toInt)
    >> List.transpose
    >> List.map List.sort
    >> List.transpose
    >> List.map (fun [a;b] -> Math.Abs(a-b))
    >> List.sum


    