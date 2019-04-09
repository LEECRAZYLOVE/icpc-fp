module ICPC
open System

let commaSprinkler (input : string) =
    let input = input.Split(' ')
    Seq.toList input

let iterate list func=
    list |> List.iter (fun x -> x |> func)

let isComma x = x = ','

let isFullStop x = x = '.'

let isWord x = x = x

let CommaFullStop (x : string) list =
    match List.tryFind isComma list,List.tryFind isFullStop list, List.head (Seq.toList x), List.findIndex isWord list = List.length list with
    |Some value, None, ',', false -> (List.tail (Seq.toList x)).ToString()
    |Some value, None, a, false -> x.TrimEnd()
    |None, Some value, a, true -> x
    |None, None, a, false -> x
    |_ -> x

let identifyCF list = 
    list |> List.iter (fun x -> CommaFullStop x)
    
let removeCharAtEnd x = 
    let rec build (word : List<string>) acc =
        match word.Length with
        | 1 -> acc
        | _ -> build (List.tail word) (acc + List.head word)
    build (Seq.toList x) " "


//failwith "Not implemented"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
