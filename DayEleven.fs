module DayEleven

let rec digitLoop acc i =
    if i = 0L then acc
    else
        digitLoop (acc+1L) (i/10L)
let digits = digitLoop 0L
let rec powLoop n acc=
    if n = 0L then acc else
    powLoop (n-1L) (acc*10L)
let pow n = powLoop n 1L



let (|Zero|_|) =
    function
    | 0L -> true
    | _ -> false
let (|EvenDigits|_|) i =
    let digs = digits i
    if digs % 2L = 0L then
        let modulus = pow (digs/2L)
        Some (i / modulus, i % modulus)
    else None

let evaluateSingle =
    function
    | Zero -> [1L]
    | EvenDigits (a,b) -> [a;b]
    | other -> [other * 2024L]

let evaluate = List.collect evaluateSingle

let rec evaluateMany n =
    if n = 0 then id else
    evaluate >> evaluateMany (n-1)

let rec countLoop (cache: Map<int * int64, int64>)  =
    function
    | [] -> cache
    | (0, num) :: rest -> countLoop (Map.add (0, num) 1 cache) rest
    | (ct, num) :: rest -> 
        match Map.tryFind (ct, num) cache with
        | Some n -> countLoop  cache rest
        | None ->
            let items = evaluateSingle num |> List.map (fun t -> (ct-1, t))            
            let notFound = items |> List.filter (fun i -> not (Map.containsKey i cache))
            if List.isEmpty notFound then
               countLoop
                   (
                       Map.add
                           (ct,num)
                           (items |> List.sumBy (fun k -> (Map.find k cache)))
                           cache)
                   rest
            else
                countLoop cache (notFound @ (ct,num) :: rest)
let count n input =
    // This is too low 🤕
    let loopInput = input |> List.map (fun i -> (n,i))
    let found = countLoop Map.empty loopInput
    loopInput |> List.sumBy (fun i -> Map.find i found )