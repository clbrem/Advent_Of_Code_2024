namespace Advent
module Array2D =
    
    let ofList<'T> items =
        let rows = List.length items
        if rows = 0 then Array2D.create 0 0 Unchecked.defaultof<'T>
        else
            let cols = List.head items |> List.length
            Array2D.init rows cols (fun i j -> items[i][j])
    let tryNext (i,j) (grid: 'T array2d)=
        if i+1 < grid.GetLength 0 then Some (i+1,j)
        elif j+1 < grid.GetLength 1 then Some (0,j+1)
        else None
    let fold f state (grid: 'T array2d)=
        let rec loop state (i,j) =
            let state' = f state (i,j) grid[i,j]                        
            match tryNext (i,j) grid with
            | Some ij -> loop state' ij
            | None -> state'
        loop state (0,0)
        
