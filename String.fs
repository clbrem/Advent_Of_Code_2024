namespace Advent
module String =
    
    let isEmpty (s: string) = System.String.IsNullOrWhiteSpace s 
    let split (splitBy: string) (s: string) =
        s.Split(splitBy)
        |> Array.toList
        |> List.filter (isEmpty >> not)
    
    let lines = split System.Environment.NewLine
    let words = split " " 
    let toInt = System.Int32.Parse
    
