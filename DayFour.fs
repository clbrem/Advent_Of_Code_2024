module DayFour
open System
open Advent

let init rows cols=
    Array2D.create rows cols Char.MinValue

let ofJagged jagged =
    let rows = List.length jagged
    let cols = List.length (List.head jagged)
    let arr = init rows cols
    for i in 0..rows-1 do
        for j in 0..cols-1 do
            arr[i,j] <- jagged[i][j]
    arr
let ofString input =
    input
    |> String.lines
    |> List.map String.trim
    |> List.map String.chars
    |> ofJagged
    
let inBounds (i,j) (arr: char array2d)=
    i < arr.GetLength 0 && i >= 0 && j < arr.GetLength 1 && j >= 0 
let letter (c: char) (i,j) (arr: char array2d) =
    inBounds (i,j) arr && arr[i,j] = c
let (|Letter|_|) (c, (i,j))= letter c (i,j) 
let (|X|_|) = letter 'X'
let (|M|_|) = letter 'M'
let (|S|_|) = letter 'S'
let (|A|_|) = letter 'A'
let neighbors c (i,j) =
    [c,(i-1,j);c,(i+1,j);c,(i,j-1);c,(i,j+1)]
let u (i,j) = (i-1, j)
let d (i,j) = (i+1, j)
let l (i,j) = (i, j-1)
let r (i,j) = (i, j+1)
let ul = u >> l
let ur = u >> r
let dl = d >> l
let dr = d >> r

let xmas (i,j) dir =
    function
    | X (i,j) & M (dir (i,j)) & A (dir (dir(i,j) )) & S (dir (dir (dir(i,j)))) -> 1
    | _ -> 0
    
let locus (i,j) (arr: char array2d)=
    List.map (xmas (i,j)) [u;d;l;r;ul;ur;dl;dr]
    |> List.fold ( fun i f -> (f arr) + i ) 0
    
let (|Main|_|) (i,j)=
    function
    | A (i,j) & M (ul (i,j)) & S (dr (i,j))
    | A (i,j) & M (dr (i,j)) & S (ul (i,j)) -> true
    | _ -> false
let (|Counter|_|) (i,j) =
    function
    | A (i,j) & M (ur (i,j)) & S (dl (i,j))
    | A (i,j) & M (dl (i,j)) & S (ur (i,j)) -> true
    | _ -> false
let xmasCounterMain (i,j) =
    function
    | Main (i,j) & Counter (i,j) -> 1
    | _ -> 0
    
    
