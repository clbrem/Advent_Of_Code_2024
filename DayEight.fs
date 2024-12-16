module DayEight
open Advent
open System

type Grid = int*int*Map<char, (int*int) list>

module Grid =
    let extent (i,j,items) =(i,j) 
    let antennae (i,j,items) = items 
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
        m, n, loop Map.empty 0 0
    let ofString input=
        let arr =
            input
            |> String.lines
            |> List.map String.trim
            |> List.map String.chars
            |> Array2D.ofList
        ofArray arr
    
    let inBounds (m, n) (i, j) =
        i >= 0 && i < m && j >= 0 && j < n
    
    let antiNode (m,n) ((i,j), (k,l)) =        
        set [( 2 * i - k, 2 * j - l ); ( 2 * k - i, 2 * l - j )]
        |> Set.filter (inBounds (m,n))
    let rec antiNodeOfList (m,n) acc=
        function
        | a :: rest ->
            List.fold (fun acc' b -> Set.union acc' (antiNode (m,n) (a,b))) acc rest
            |> antiNodeOfList (m,n) 
            <|rest
        | _ -> acc
    let antiNodes (g: Grid)=
        Map.fold (fun acc _ -> antiNodeOfList (extent g) acc) Set.empty (antennae g) 
        
    let collinear (i,j) (k,l) (m,n) =
        if k = i then
            m = k
        else
          (l-j) * (m-i) = (n-j) * (k-i)
    
    let rec linSpaceOfList (acc: Set<(int*int)*(int*int)> )=
        function
        | a :: rest ->
            List.fold (fun acc' b -> Set.add (a,b) acc') acc rest
            |> linSpaceOfList 
            <| rest
        | [] -> acc
    
    let linSpace (g: Grid)=
        Map.fold (fun acc _ -> linSpaceOfList acc) Set.empty (antennae g)
    
    let test linSpace (i,j) =
        Set.fold (fun acc (a,b)  -> acc || (collinear a b (i,j))) false linSpace
    