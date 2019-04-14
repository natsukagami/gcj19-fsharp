open System

let repeat = 18

let mills = [4; 3; 5; 7; 11; 13; 17] |> List.rev

let ask lst = 
    printfn "%s" (lst |> List.map string |> String.concat " ")
    Console.ReadLine().Split [|' '|] |> Array.toList |> List.map int

let askMill num = 
    List.replicate repeat num
    |> ask
    |> List.sum
    |> fun x -> x % num

let takeMod num = mills |> List.map (fun x -> num % x)

let solve () = 
    let res = mills |> List.map askMill

    let fst = List.head res
    [fst .. 17 .. 1000001]
    |> List.find (fun x -> (takeMod x) = res)

let [|T; N; M|] = Console.ReadLine().Split [|' '|] |> Array.map int

[1 .. T]
|> List.map (fun _ -> printfn "%d" (solve()); Console.ReadLine() |> ignore)