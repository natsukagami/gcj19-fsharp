open System

let T = int (Console.ReadLine ())

let solve _ (str: string) = 
    let rec run = function
      | [] -> []
      | 'S' :: rest -> 'E' :: (run rest)
      | x :: rest -> 'S' :: (run rest)
    
    str.ToCharArray()
    |> Array.toList 
    |> run 
    |> List.map string 
    |> String.concat ""

let sols = 
    [1..T]
    |> List.map (fun _ -> solve (Console.ReadLine()) (Console.ReadLine()))

[1..T]
|> List.zip sols
|> List.map (fun (sol, case) -> printfn "Case #%d: %s" case sol)
|> ignore