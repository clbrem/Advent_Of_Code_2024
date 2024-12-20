module DayTen
open System
open Advent

type TopoMap = int array2d
    
module TopoMap =
    let ofString =
        String.lines
        >> List.map (String.chars >> List.filter Char.IsDigit >> List.map (fun c -> int c - int '0'))
        >> Array2D.ofList
    
    let rec trailHeads (map:TopoMap) =
        let m,n = Array2D.length1 map, Array2D.length2 map
        [ for i in 0..m-1 do
          for j in 0..n-1 do
            if map.[i,j] = 0 then yield (i,j) ]
    
    let neighbors (map:TopoMap) (i,j)  =
        let m,n = Array2D.length1 map, Array2D.length2 map 
        let inBounds (i,j) =
           i >= 0 && i < m && j >= 0 && j < n
        let next = map.[i,j] + 1
        [ for k,l in [(i+1, j); (i-1, j); (i, j+1); (i, j-1) ] do
          if inBounds (k,l)  && map[k,l] = next then yield (k,l) ]
    let rec peaks  (map:TopoMap) (i,j) =
        if map[i,j] = 9 then set [(i,j)]
        else
            Set.unionMany [for n in neighbors map (i,j) -> peaks map n]
    
    let rec peakLoop  (map:TopoMap) (acc: Map<int*int, Set<int*int>>)   =
        function
        | (i,j) :: rest ->
            if map.[i,j] = 9 then Map.add (i,j) (set [(i,j)]) acc |> peakLoop map <| rest
            else
                let nbs = neighbors map (i,j) 
                let unresolved = nbs |> List.filter (fun n -> not (Map.containsKey n acc))
                if unresolved |> List.isEmpty then
                    nbs
                    |> List.map (fun n -> Map.find n acc)
                    |> Set.unionMany                    
                    |> Map.add (i,j)
                    <| acc
                    |> peakLoop map
                    <| rest
                else peakLoop map acc <| unresolved @ (i,j) :: rest
        | _ -> acc
    let peaks2 map =
        let trailHeads = trailHeads map        
        let found =peakLoop map Map.empty trailHeads
        trailHeads
        |> List.sumBy (fun m -> Map.find m found |> Set.count)
    let rec ratingLoop  (map:TopoMap) (acc: Map<int*int, int>)   =
        function
        | (i,j) :: rest ->
            if map.[i,j] = 9 then Map.add (i,j) (1) acc |> ratingLoop map <| rest
            else
                let nbs = neighbors map (i,j) 
                let unresolved = nbs |> List.filter (fun n -> not (Map.containsKey n acc))
                if unresolved |> List.isEmpty then
                    nbs
                    |> List.map (fun n -> Map.find n acc)
                    |> List.sum
                    |> Map.add (i,j)
                    <| acc
                    |> ratingLoop map
                    <| rest
                else ratingLoop map acc <| unresolved @ (i,j) :: rest
        | _ -> acc
    let ratings map =
        let trailHeads = trailHeads map        
        let found =ratingLoop map Map.empty trailHeads
        trailHeads
        |> List.sumBy (fun m -> Map.find m found )