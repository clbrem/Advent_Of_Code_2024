module DayTwo
open Advent

let parse =
    String.lines
    >> List.map (String.words >> List.map String.toInt)

type Level =
    | Unsafe
    | Start
    | Unknown of int
    | Increasing of int
    | Decreasing of int        
let private testAdjacent =
    function
    | Unsafe -> fun _ -> Unsafe
    | Start -> Unknown    
    | Unknown i -> 
        function
        | j when (j > i) && (j - i) <= 3 -> Increasing j        
        | j when (j < i) && (i - j) <= 3 -> Decreasing j
        | _ -> Unsafe
    | Increasing i ->
        function
        | j when (j > i) && (j - i) <= 3 -> Increasing j
        | _ -> Unsafe
    | Decreasing i ->
        function
        | j when (j < i) && (i-j) <= 3 -> Decreasing j
        | _ -> Unsafe       

    
let test =
    List.fold testAdjacent Start
let punchIt =
    List.map test
    >> List.sumBy (function | Unsafe -> 0 | _ -> 1)
    
      
          
    
