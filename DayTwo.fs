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


let testy2 items =
    let n = List.length items
    let rec loop  =
        function
        | i when i = n -> Unsafe
        | i -> match List.removeAt i items |> test with
                | Unsafe -> loop (i+1)
                | other -> other
    match test items with
    | Unsafe -> loop 0
    | other -> other

let testAdjacent2 :bool * Level * Level -> int -> bool*Level*Level=
    function
    | true, _, curr -> fun i -> true, curr, testAdjacent curr i
    | false, prev, curr ->
            fun i ->
                testAdjacent curr i         
                |> function
                    | Unsafe ->
                        match testAdjacent prev i with
                        | Unsafe ->
                            true,Unsafe, curr
                        | other -> true, Unsafe, other
                    | other -> false, curr, other
let test2 =
    List.fold testAdjacent2 (false,Start,Start)
    >> fun (_,_,item) -> item
let punchIt =
    List.map test
    >> List.sumBy (function | Unsafe -> 0 | _ -> 1)
    
let punchIt2=
    List.map test2    
    >> List.sumBy (function | Unsafe -> 0 | _ -> 1)
          
    
