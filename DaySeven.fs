module DaySeven
open Advent

type Equation =
    int * int list

module Equation =
    let ofString =
        String.split ":"
        >> List.map String.trim
        >> function
            | [a;b] -> (a |> String.toInt, b |> String.split " " |> List.map String.toInt |> List.rev)
            | _ -> failwith "Invalid input"
    
    let iterate  =
        function
        | rhs, a :: rest ->
            false, false, [ if rhs % a = 0 then yield rhs / a, rest; yield rhs - a, rest ]            
        | 0, [] -> true, true, [] 
        | _, [] -> true, false, []
    let bind (f: 'a -> bool * bool * 'a list) (_: bool, a: bool, b: 'a list) =    
        List.map f b
        |> List.fold (fun (finished, a, items) (status, b, moreItems) -> finished && status, a || b, List.append items moreItems) (true, a, [])    
    let ret (a: 'a) = false, true, [a]
        
    let rec solveLoop (finished: bool, solvable:bool, eqs: Equation list) =
        if finished then solvable
        else (finished,solvable, eqs) |> bind iterate |> solveLoop
    let canSolve = ret >> solveLoop
