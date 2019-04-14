open System 

type Node = { items: int list; children: Node option array }

let changeArray pos f array = 
  array
  |> Array.mapi (fun i v -> if i = pos then f v else v)

let rec add id chars node = 
  let { items = items; children = children } = defaultArg node { items = []; children = Array.replicate 26 None }   
  match chars with
  | [] -> Some { items = id :: items; children = children }
  | x :: rst -> 
    Some { items = id :: items; children = changeArray x (add id rst) children }

let dfs node = 
  let rec run depth cur = function
    | None -> cur
    | Some(v) -> v.children |> Array.fold (run (depth + 1)) (if depth > 0 then (v, depth) :: cur else cur)
  run 0 [] node

let bfs node = 
  let rec bfsStep depth ans next = function 
    | [] -> if List.isEmpty next then ans else bfsStep (depth + 1) ans [] next 
    | None :: rst -> bfsStep depth ans next rst 
    | Some v :: rst -> 
      let ans = if depth = 0 then ans else v :: ans 
      bfsStep depth ans (List.append (v.children |> Array.toList) next) rst
  bfsStep 0 [] [] [node]

let solve (n: string) = 
  let n = int n
  let strings = [1 .. n] |> List.map (fun _ -> Console.ReadLine().ToCharArray() |> Array.rev |> Array.toList |> List.map (fun x -> (int x) - (int 'A')))
  // eprintfn "%A" strings
  let root = List.zip [1..n] strings |> List.fold (fun node (id, chars) -> add id chars node) None
  let nodes = dfs root |> List.sortBy snd |> List.rev
  // eprintfn "%A" nodes
  let resolve (set: Set<int>) (node, depth) = 
    let lx = node.items |> List.filter (fun x -> not <| Set.contains x set)
    // eprintfn "%d %A" depth lx
    match lx with 
    | a :: b :: _ -> (set |> Set.add a |> Set.add b)
    | _ -> set
  let ansSet = List.fold resolve Set.empty nodes
  Set.count ansSet

let T = int <| Console.ReadLine()

[1..T]
|> List.iter (fun case -> printfn "Case #%d: %d" case (solve (Console.ReadLine())))