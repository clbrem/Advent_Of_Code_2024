module DayNine
open Advent
type File =
    { id: int
      start: int64
      stop: int64
    }    
type Free =
    { start: int64
      stop: int64
    }
type Block = | File of File | Free of Free
module Block =
    let checksum  =
        function
        | File { id=id; start=start; stop=stop } -> (int64  id) * (stop*(stop-1L)/2L - start*(start-1L)/2L)
        | _ -> 0
    let length = function
        | File f -> f.stop - f.start
        | Free f -> f.stop - f.start
    let rec private parseLoop acc locationCursor idCursor isFile=
        function
        | [] -> acc |> List.rev
        | a :: rest ->        
            if isFile then
                parseLoop
                    (File {id=idCursor; start=locationCursor; stop=locationCursor+a} :: acc)
                    (locationCursor+a)
                    (idCursor+1) false rest
            else
                parseLoop
                    (Free {start=locationCursor; stop=locationCursor+a} :: acc)
                    (locationCursor+a)
                    idCursor true rest
    let ofString =
        String.chars
        >> List.map (fun c -> int64 c - int64 '0')
        >> parseLoop [] 0L 0 true