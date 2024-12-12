module DayFive
open System
open System.Text.RegularExpressions
open Advent
type Rules = Map<int,int Set>
type Sequence = int list

module Sequence =
    let middle items=
        let n = List.length items
        items[(n-1)/2]
    let ofString (s: string) =
        String.split "," s |> List.map Int32.Parse

module Rules =
    let parseLine line =
        let matched = Regex("(\d+)\|(\d+)").Match(line)
        Int32.Parse matched.Groups.[1].Value, Int32.Parse matched.Groups.[2].Value
    
    let ofString (s: string) : Rules =
        String.lines s        
        |> List.map parseLine
        |> List.fold (fun acc (k,v) -> 
            match Map.tryFind k acc with
            | Some set -> Map.add k (Set.add v set) acc
            | None -> Map.add k (Set.singleton v) acc
        ) Map.empty
        
    let check (rules: Rules) (encountered: Set<int>, valid: bool) (i: int) =
        match rules |> Map.tryFind i with
        | Some shouldBeLater ->
            (
                Set.add i encountered,
                valid && Set.intersect shouldBeLater encountered |> Set.isEmpty
            )
        | None -> (Set.add i encountered, valid)
    let checkSequence rules (sequence: Sequence)=
        if (List.fold (check rules) (Set.empty, true) sequence |> snd) then
            Some (Sequence.middle sequence)
        else
            None
    let expand inputs rules i =
        match rules |> Map.tryFind i with
        | Some set -> Set.intersect inputs set |> Set.toList |> List.map (fun x -> (false,x))
        | None -> []
    let rec loop (inputs: int Set) (rules: Rules) (visited: int Set) acc =
       function
       | [] -> acc
       | (_,a) :: rest when Set.contains a visited -> loop inputs rules visited acc rest
       | (true,a) :: rest -> loop inputs rules (Set.add a visited) (a::acc) rest
       | (false,a) :: rest ->
           loop inputs
                rules
                visited
                acc
                ((true, a) :: rest |> List.append (expand inputs rules a))
    let reSort (rules: Rules)(inputs: Sequence)  =
        loop (Set.ofList inputs) rules Set.empty [] (List.map (fun x -> (false,x)) inputs)
    
    let resortSequence rules (sequence: Sequence)=
        if (List.fold (check rules) (Set.empty, true) sequence |> snd) then
            None
        else
            Some (reSort rules sequence |> Sequence.middle)