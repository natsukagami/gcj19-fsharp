open System

let ask (seq: char list) = 
    printfn "%s" (seq |> List.map string |> String.concat "")
    Console.ReadLine().ToCharArray() |> Array.toList

let scan scanFn state lst = 
    let rec run cur state = function 
        | [] -> (List.rev cur, state)
        | x :: rest -> 
            let (item, newState) = scanFn state x 
            run (item :: cur) newState rest
    run [] state lst
    

let firstPass n = 
    let groups = [0 .. (n / 16)]
    let sizes = groups |> List.map (fun x -> min 16 (n - 16 * x))
    let res = 
        sizes // Take at most 16 items
        |> List.mapi (fun index x -> List.init x (fun _ -> (char ((int '0') + (index % 2)))))
        |> List.concat
        |> ask
    sizes
    |> scan (fun remain _ -> 
            match remain with 
            | [] -> (0, [])
            | x :: rest -> (List.length (List.takeWhile ((=) x) remain), List.skipWhile ((=) x) rest)
        ) res
    |> fst

let rec compute offset n lst = 
    // eprintfn "%d %A" offset lst
    if offset = 1 then 
        // Return the result
        lst 
        |> List.take n
        |> List.zip [0..n-1]
        |> List.filter (snd >> (=) 0)
        |> List.map fst
    else
        let offset = offset / 2
        let result = 
            [0 .. n - 1]
            |> List.chunkBySize offset
            |> List.map List.length
            |> List.mapi (fun id ln -> List.replicate ln (if id % 2 = 1 then '1' else '0'))
            |> List.concat
            |> ask
        lst
        |> scan (fun set n -> (List.take n set, List.skip n set)) result
        |> fst
        |> List.collect (fun l -> 
            let total = List.length l 
            let zeros = List.length (List.takeWhile ((=) '0') l)
            [zeros; total - zeros]
        )
        |> compute offset n

// let remainingPasses n 

let solve (input: string) = 
    let n :: _ = input.Split([|' '|]) |> Array.toList |> List.map int
    let f = firstPass n |> compute 16 n 
    f |> List.map string |> String.concat " " |> printfn "%s"
    Console.ReadLine() |> ignore    

let T = int (Console.ReadLine ())

[1 .. T]
|> List.iter (fun _ -> solve (Console.ReadLine()))