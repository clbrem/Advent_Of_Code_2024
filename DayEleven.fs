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

