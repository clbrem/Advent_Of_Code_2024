module DayThree
open System
open System.Text.RegularExpressions
open Advent
let private parseMul input=
    let v = Regex("^mul\((\d+)\,(\d+)\)$").Match(input)
    if v.Success
    then String.toInt(v.Groups[1].Value) * String.toInt(v.Groups[2].Value)
    else 0
let mul input =
    let matched = Regex("mul\((\d+)\,(\d+)\)").Matches(input)
    matched
    |> List.ofSeq
    |> List.map (fun mm -> String.toInt(mm.Groups.[1].Value) * String.toInt(mm.Groups.[2].Value))
    |> List.sum
let mulRegex = Regex("^mul\((\d+)\,(\d+)\)")
let doRegex = Regex("^do\(\)")
let dontRegex = Regex("^don\'t\(\)")
let (|RegEx|_|) (regex:Regex) (input:string)=
    let m = regex.Match(input)
    if m.Success then Some m else None
let cursorShift (matched: Match)  =
    matched.Index + matched.Length
let (|Mul|Do|Dont|Next|) = 
    function
    | RegEx mulRegex matched ->         
        Mul (
            cursorShift matched,            
            String.toInt (matched.Groups[1].Value) * String.toInt(matched.Groups[2].Value)
            )
    | RegEx doRegex matched ->         
        Do (
            cursorShift matched
            )
    | RegEx dontRegex matched ->
        Dont (
            cursorShift matched            
            )
    | _ -> Next 
let rec loopMul n acc (switch, input)=
    match switch, input with
    | _, "" -> acc    
    | switch, Next ->    
        loopMul n acc (switch, input.Substring 1)
    | true,(Mul(i,product) & input)->
        loopMul n (acc + product) (true, input.Substring i)
    | false,Mul(i, _)  ->
        loopMul n acc (false, input.Substring i)
    | _, Do i  ->
        loopMul n acc (true, input.Substring i)
    | _, Dont i ->
        loopMul n acc (false,  input.Substring i)                

let mul2 input =
    let n = String.length input
    loopMul n 0 (true, input)