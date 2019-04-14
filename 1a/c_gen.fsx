let gen () = 
    printfn "1000"
    [1..1000]
    |> List.map ((fun v -> List.init 50 (fun u -> char ((v + u) % 26) + 'A')) >> (List.map string) >> String.concat "")
    |> List.iter (printfn "%s")

printfn "100"
[1..100] |> List.iter (fun _ -> gen())