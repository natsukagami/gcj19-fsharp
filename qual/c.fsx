open System.Numerics
open System

let rec gcd (a: BigInteger) (b: BigInteger) = 
  if b = BigInteger.Zero then 
    a
  else
    gcd b (BigInteger.Remainder(a, b))

let resolveList lst = 
  let rec gcdList last = function 
    | [] -> [last]
    | x :: rest -> last :: (gcdList (BigInteger.Divide(x, last)) rest)
  let rec firstNonRepeat cnt = function 
    | a :: (b :: _ as rest) when a = b -> firstNonRepeat (cnt + 1) rest
    | x -> (cnt, x)
  let rec refillSwap a b lst = function 
    | 0 -> lst
    | x -> refillSwap b a (a :: lst) (x - 1)
  let (repeats, lst) = firstNonRepeat 0 lst
  match lst with 
    | a :: b :: _ -> 
        let x = gcd a b
        let y = (BigInteger.Divide(a, x))
        refillSwap x y (gcdList y lst) repeats
    | _ -> failwith "Oh no D:"
  

type Transform = 
  | Yes of char
  | No of BigInteger

let solve _ (input: string) = 
  let input = input.Split([|' '|]) |> Array.toList |> List.map BigInteger.Parse
  let allNums = resolveList input
//   eprintfn "%A" allNums
  let setOf = Set allNums |> Set.toSeq |> Seq.zip (Seq.initInfinite id) 
  setOf
  |> Seq.take 26
  |> Seq.toList
  |> List.fold 
    (fun nums (id, v) -> 
        nums
        |> List.map (function | No(x) when v = x -> Yes(char (id + (int 'A'))) | x -> x))
    (allNums |> List.map No)
  |> List.map (function | Yes(v) -> string v | _ -> failwith "Should have all numbers")
  |> String.concat ""

let T = int (Console.ReadLine ())

[1 .. T]
|> List.map (fun _ -> solve (Console.ReadLine ()) (Console.ReadLine()))
|> List.zip [1 .. T]
|> List.map (fun (case, sol) -> printfn "Case #%d: %s" case sol)
|> ignore
