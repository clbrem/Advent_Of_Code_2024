namespace Advent
open System
module String =
    
    let isEmpty (s: string) = System.String.IsNullOrWhiteSpace s 
    let split (splitBy: string) (s: string) =
        s.Split(splitBy)        
        |> Array.toList
        |> List.filter (isEmpty >> not)
    
    let lines = split Environment.NewLine
    let words = split " " 
    let toInt = System.Int32.Parse
    let toInt64 = Int64.Parse
    let chars (s: string)  = s |> List.ofSeq
    let trim (s: string) = s.Trim()
