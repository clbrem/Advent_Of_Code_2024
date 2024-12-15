module DayEight
open Advent
open System

type Grid = int*int*Map<char, (int*int) list>

module Grid =
    let antenna (c: char) =
        if Char.IsDigit c || Char.IsLetter c then
            Some c
        else None
    let private addToMap (m: Map<char, (int*int) list>) c (i, j) =
        match Map.tryFind c m with
        | Some l -> Map.add c ((i, j)::l) m
        | None -> Map.add c [(i, j)] m
            
    let ofArray (input: char array2d) =
        let m = Array2D.length1 input
        let n = Array2D.length2 input
        let rec loop acc i j =
            if i >= m then acc
            elif j >= n then loop acc (i+1) 0
            else
                match antenna input.[i, j] with
                | Some c -> loop (addToMap acc c (i, j)) i (j+1)
                | None -> loop acc i (j+1)
        loop Map.empty 0 0
    let ofString =
        String.lines
        >> List.map String.trim
        >> List.map String.chars
        >> Array2D.ofList
        >> ofArray


    
