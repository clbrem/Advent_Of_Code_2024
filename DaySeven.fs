module DaySeven
open System
open Advent

type Equation =
    int64 * int64 list

module Int64 =
    let private pow (power:int64) =
        if power >= 0L then  
            let rec loop acc power =
                if power = 0L then acc
                else loop (acc * 10L) (power - 1L)
            loop 1L power
        else
            new ArgumentException ("power must be positive") |> raise
    let rec private loop acc input =
        if input  = 0L then acc 
        else loop (acc + 1L) (input / 10L)
    let digits =
      loop 0
    let modulus =
       digits >> pow
       

module Equation =
    let rec private digitLoop acc input =
        if input  = 0L then acc 
        else digitLoop (acc + 1L) (input / 10L)
    let digits = digitLoop 0
    let testValue (rhs, _) = rhs
    let ofString =
        String.split ":"
        >> List.map String.trim
        >> function
            | [a;b] -> (a |> String.toInt64, b |> String.split " " |> List.map String.toInt64 |> List.rev)
            | _ -> failwith "Invalid input"
    
    let iterate =
        function
        | rhs, a :: rest ->
            let eqs = [yield rhs - a, rest; if rhs % a = 0L then yield rhs / a, rest;]
            if eqs |> List.length > 1 then
                ()
            false, eqs
        | 0L, [] -> true, [] 
        | _, [] -> false, []
    let bind (f: 'a -> bool * 'a list) ( b: bool, items: 'a list) =    
        List.map f items
        |> List.fold (fun (solvable,items ) (solvable',items') -> solvable || solvable', items @ items') (b,[])    
    let ret (a: 'a) = false, [a]    
    let rec solveLoop  op =
        function
        | solvable, [] -> solvable
        | other -> other |> bind op |> solveLoop op  
    let canSolve = ret >> solveLoop iterate

    let iterateWithConcat =
        function
        | rhs, a :: rest ->
            let modulus = Int64.modulus a
            let eqs = [
                yield rhs - a, rest
                if rhs % a = 0L then yield rhs / a, rest 
                if (rhs - a) % modulus = 0L then yield (rhs - a) / modulus, rest 
            ]
            if eqs |> List.length > 1 then
                ()
            false, eqs
        | 0L, [] -> true, [] 
        | _, [] -> false, []
      
    let canSolveWithConcat = ret >> solveLoop iterateWithConcat