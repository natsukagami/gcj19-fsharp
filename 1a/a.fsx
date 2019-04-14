open System

let allPairs a b = 
    a 
    |> List.collect (fun x -> b |> List.map (fun y -> (x, y)))

let orElseWith f = function 
    | None -> f () 
    | v -> v

let genCells n m = 
    [1..n]
    |> List.collect (fun x -> List.init m (fun y -> (x, y + 1)))

let check (a, b) (c, d) =
    (a <> c) && (b <> d) && ((a - b) <> (c - d)) && ((a + b) <> (c + d))

let rec backtrack cur lst =
    match lst with 
    | [] -> Some cur 
    | _ ->
        let rec tryRemove pref = function 
            | x :: rst when (match cur with | [] -> true | y :: _ -> check x y)-> 
                backtrack (x :: cur) (List.append pref rst)
                |> orElseWith (fun _ -> tryRemove (x :: pref) rst)
            | [] -> None 
            | x :: rst -> tryRemove (x :: pref) rst
        tryRemove [] lst

let trySmall =
    [1..5]
    |> allPairs [1..5]
    |> List.map (fun (x, y) -> backtrack [] (genCells x y))

let gen2 row m = 
    let nx k = if k + 2 > m then k + 2 - m else k + 2
    [m .. -1 .. 1]
    |> List.collect (fun v -> [(row, v); (row + 1, nx v)])

let gen3 row m = 
    let nx k = if k + 2 > m then k + 2 - m else k + 2
    [1 .. m]
    |> List.collect (fun v -> [(row, v); (row + 1, nx v); (row + 2, v)])
    |> List.permute (fun i -> if i = 0 then 3 * m - 1 else i - 1)

let construct n m = 
    if n <= 5 && m <= 5 then 
        trySmall |> List.skip (5 * (n - 1) + (m - 1)) |> List.head
    else
        let (n, m, swap) = if (n < m) then (n, m, false) else (m, n, true)
        if n = 1 then None 
        else if n % 2 = 1 then 
            Some (List.append (gen3 1 m) ([4..2..n] |> List.collect (fun x -> gen2 x m)))
        else Some ([1..2..n] |> List.collect (fun x -> gen2 x m))
        |> Option.map (fun x -> if swap then x |> List.map (fun (x, y) -> (y, x)) else x)

let T = int <| Console.ReadLine()

let test lst = 
    let fold (set, all) item = 
        if (match all with | [] -> true | x :: _ -> check x item) then 
            (set |> Set.add item, item :: all)
        else failwith "FUCK"
    List.fold fold (Set.empty, []) lst |> ignore

// List.zip [1..400] (allPairs [1..20] [1..20])
[1..T]
|> List.map (fun _ -> Console.ReadLine().Split [|' '|] |> Array.toList |> List.map int)
|> List.mapi (fun i [a; b] -> (i + 1, (a,b)))
|> List.map (fun (i, (a, b)) -> (i, construct a b))
|> List.iter (function 
    | (i, None) -> printfn "Case #%d: IMPOSSIBLE" i
    | (i, Some(l)) -> test l; printfn "Case #%d: POSSIBLE" i; (l |> List.iter (fun (a, b) -> printfn "%d %d" a b)))