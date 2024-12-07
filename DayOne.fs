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

let (|Counter|) items=
    Counter (
        items
        |> List.countBy id
        |> Map.ofList
    )
    

let punchIt2 = 
    String.lines
    >> List.map (String.words >> List.map String.toInt)
    >> List.transpose
    >> function
        | [items; Counter counter] ->
            items |> List.sumBy (fun i -> i * (Map.tryFind i counter |> Option.defaultValue 0))
        | _ -> 0
   