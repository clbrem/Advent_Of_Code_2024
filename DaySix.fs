module DaySix
open Advent
type Direction = 
    | North
    | East
    | South
    | West
type Guard = { coord: int*int; direction: Direction }

type Grid = char array2d 
module Grid =
    let ofString =
        String.lines
        >> List.map String.trim
        >> List.map String.chars        
        >> Array2D.ofList
type Visited = Set<int*int>

let outOfBounds (grid: Grid) (x,y) = x < 0 || x >= grid.GetLength 1 || y < 0 || y >= grid.GetLength 0

let (|Guard|_|) (grid: Grid) (x,y) = 
    if outOfBounds grid (x,y) then None
    else        
        match grid[y,x] with
        | '^' -> Some { coord = (x,y); direction = North }         
        | 'v' -> Some {coord = (x,y); direction = South } 
        | '<' -> Some {coord = (x,y); direction = West }
        | '>' -> Some {coord = (x,y); direction = East }
        | _ -> None
let (|Barrier|_|) (grid: Grid) (x,y) = 
    if outOfBounds grid (x,y) then false
    else        
        match grid[y,x] with
        | '#' -> true
        | _ -> false
let rec private findLoop grid (i,j) =
    match (i,j) with
    | Guard grid guard -> Some guard
    | _ ->
        match Array2D.tryNext (i,j) grid with
        | Some (l,m) -> findLoop grid (l,m)
        | None -> None
let findGuard grid = findLoop grid (0,0)

let peek grid guard =
    let x,y = guard.coord
    let dx,dy = 
        match guard.direction with
        | North -> (0,-1)
        | East -> (1,0)
        | South -> (0,1)
        | West -> (-1,0)
    let nxt = (x+dx, y+dy)
    if outOfBounds grid nxt then None
    else Some nxt
let turn guard = 
    match guard.direction with
    | North -> { guard with direction = East }
    | East -> { guard with direction = South }
    | South -> { guard with direction = West }
    | West -> { guard with direction = North }
let rec private visit grid (guard: Guard) visited  =    
    match peek grid guard with
    | None -> Set.add guard.coord visited
    | Some (Barrier grid) ->
        Set.add guard.coord visited
        |> visit grid (turn guard)
    | Some (i,j) ->
        Set.add guard.coord visited
        |> visit grid { guard with coord = (i,j) }
    
        
let traverse grid guard =
    visit grid  guard Set.empty