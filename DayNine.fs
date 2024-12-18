module DayNine
open Advent
type File =
    { id: int
      start: int64
      stop: int64
    }
module File =
    let checksum  =
        function
        | { id=id; start=start; stop=stop } -> (int64  id) * (stop*(stop-1L)/2L - start*(start-1L)/2L)
        
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
        | [] ->  acc |> List.rev, acc
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

type Memory = Block list * Block list
module Memory =
    let combobulate (free: Free) (file: File)=
        let fileLength = Block.length (File file)
        let freeLength = Block.length (Free free)
        if fileLength > freeLength then
           {file with start = free.start; stop = free.stop} |> File.checksum ,
           File {file with stop = file.stop - freeLength} |> Some 
        elif fileLength = freeLength then
            {file with start = free.start; stop = free.stop} |> File.checksum ,
            None
        else
            {file with start = free.start; stop = free.start + fileLength}|> File.checksum ,
            Free {free with start = free.start + fileLength; stop = free.stop} |> Some
    let decombobulate (free: Free) (file: File)=
        let fileLength = Block.length (File file)
        let freeLength = Block.length (Free free)
        if fileLength > freeLength then
           file |> File.checksum ,
           None
        elif fileLength = freeLength then
            {file with start = free.start; stop = free.stop} |> File.checksum ,
            None
        else
            {file with start = free.start; stop = free.start + fileLength} |> File.checksum ,
            Free {free with start = free.start + fileLength; stop = free.stop} |> Some        
        
    let rec private loop combobulator (acc: int64) memory: int64 =
        match memory with
        | [],[] -> acc
        | File a :: _, File b :: _ when a.id = b.id -> acc + (File.checksum b)
        | File a :: _, File b :: _ when a.id > b.id -> acc
        | left, Free _ :: right -> loop combobulator acc (left, right)
        | File a :: left, right -> loop combobulator (acc + (File.checksum a))  (left,right)
        | Free a :: left, File b :: right ->
            match combobulator a b with
            | a', Some (Free free) -> loop combobulator (acc + a') (Free free :: left, right)
            | a', Some (File file) -> loop combobulator (acc + a') (left, File file :: right)
            | a', None -> loop combobulator (acc + a') (left, right)
        | _, _ -> acc
    
    let rec private scan (id: int) free acc =
        let n = Block.length (Free free)
        function
        | [] -> None
        | Free _ :: rest -> scan id  free acc rest 
        | File a :: _ when a.id <= id -> None
        | File a :: rest when Block.length (File a) <= n -> Some (acc , a, rest) 
        | File a :: rest ->  scan id free (acc + (File.checksum a)) rest

    let rec private loopB (id: int) (acc: int64) memory: int64 =
        match memory with
        | [],[] -> acc
        | File a :: _, File b :: _ when a.id = b.id -> acc + (File.checksum b)
        | File a :: _, File b :: _ when a.id > b.id -> acc
        | left, Free _ :: right -> loopB id acc (left, right)
        | File a :: left, right -> loopB a.id (acc + (File.checksum a))  (left,right)
        | Free a :: left,  right ->
            match scan id a 0L right with
            | Some (acc', b, rest) ->
                let last = {b with start = a.start; stop = a.start + Block.length (File b)} |> File.checksum
                let fr = Free {a with start = a.start + Block.length (File b)}
                loopB id (acc + acc' + last) (fr:: left, rest)
            | None -> loopB id acc (left, right)                    
        | _,_ -> acc
    let partA  = loop combobulate 0L
    let partB = loopB 0 0
            