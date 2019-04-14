let N = int (System.Console.ReadLine())

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let solve caseNo str = 
  let pad l r (a, b) = 
    (l :: a, r :: b)

  let rec run = function 
    | '4' :: rest -> pad '1' '3' (run rest)
    | x :: rest -> pad x '0' (run rest)
    | [] -> [], []

  let (org, nw) = run str
  let org = org |> List.map string |> String.concat ""
  let nw  = nw  |> List.map string |> String.concat "" |> (fun x -> x.TrimStart([|'0'|]))
  printfn "Case #%d: %s %s" caseNo org nw

[1 .. N]
|> List.map (fun x -> solve x (System.Console.ReadLine().ToCharArray() |> Array.toList))
|> ignore
