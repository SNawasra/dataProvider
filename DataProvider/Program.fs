// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System;
open Chessie.ErrorHandling;
open FSharp.Core;
open FSharp.Data;

type String15 = 
    |String15 of string   
    override this.ToString () = 
       match this with 
        | String15 (x) -> sprintf "%s" x
    
type String30 =
    |String30 of string   
    override this.ToString () = 
       match this with 
        | String30 (x) -> sprintf "%s" x

type String11 = 
    |String11 of string   
    override this.ToString () = 
       match this with 
        | String11 (x) -> sprintf "%s" x

let stringEmptyNull s = 
    if String.IsNullOrEmpty(s) then
        warn "Null or empty string" <| s
    else
        s|> pass 

let StringMax15 s = 
    if String.length s > 15 then
        fail "exeeded max length 15" //<|String15 s
    else
        pass <| String15 s

let StringMax11 s = 
    if String.length s > 11 then
        fail "exeeded max length 15" //<|String15 s
    else
        pass <| String11 s

let StringMax30 s = 
    if String.length s > 30 then
        fail "exeeded max length 15" //<|String15 s
    else
        pass <| String30 s

let removMinus(x:string):string = 
    x.Remove(0,1)

let checkForMinus (x:string):bool = 
    x.Contains("-")

let vailidAccountNum s  = 
    if checkForMinus s then
        let sub = removMinus s
        s|> pass
    else
        pass <| s

let csv = new CsvProvider<"data.csv">()

type InpSchema = {Batch:string;Date:DateTime;AccountNumber:string;``JE Reference``:string;``Discribution Reference``:string; }
type Sample = {BatchNumber:String15;SourceDocument:String11 option;JERef:String30 }

let validateBatch =  StringMax15
let validateAccountNumber x =  lift Some (StringMax11 x)  
let validateDiscripRef = StringMax30


let unsafeInput =
    csv.Rows
    |> Seq.map(fun x -> {Batch=x.Batch; Date=x.Date;AccountNumber=x.AccountNumber;``JE Reference``=x.``JE Reference``;``Discribution Reference``=x.``Distribution Reference``} )


let inputValidation (x:InpSchema) =
    trial {
     let! batchNumber = validateBatch x.Batch
     let! accountNum =  validateAccountNumber x.``Discribution Reference``  
     let! validDiscripRef = validateDiscripRef x.``Discribution Reference``  
    return {BatchNumber=batchNumber;SourceDocument=accountNum;JERef=validDiscripRef} 
    }
    
let safeInput =
    unsafeInput 
    |> Seq.map(fun x -> inputValidation x )


//let getJref x = 
//    match x  with
//    | Some i -> printfn "%s" (i.ToString())
//    | None -> printfn "Null"

let PrintResults x =
    printfn "%s, %A, %s" (x.BatchNumber.ToString()) ( (x.SourceDocument)) (x.JERef.ToString())


let safeInput' = safeInput |> Seq.iter(fun x -> (PrintResults <!> x) |> ignore  )

// MyCsvType = CsvProvider<Schema = "BatchNumber (string), SourceDocument (string), JERef (string)", HasHeaders=false>
type MyCsvType = CsvProvider<
                Sample="BatchNumber, SourceDocument, JERef", 
                Schema = "BatchNumber (string), SourceDocument (string), JERef (string)", 
                HasHeaders = true>
//let ToString s =
//    match s with
//    | Ok(v,msgs) -> sprintf "OK: %A - %s" v (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))
//    | Bad(msgs) -> sprintf "Error: %s" (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))    

let mapToCsvType x =
    MyCsvType.Row(string x.BatchNumber, string x.JERef,"")

//let  mapToCsvList xs  =
//       printfn "got to start "
//       let rec loop xs acum=
//           match xs with 
//           | h::t -> 
//               printfn "this is the HEAD:  %A \r\n " h
//               printfn "this is the Tail:  %A \r\n " t 
//               match h with 
//               |Pass z ->                                        
//                   loop t ((mapToCsvType z)::acum)  
//               | _ -> loop t acum                
//           | [] ->                
//               acum
//       loop xs []                  


let isSucess inp =  not(failed inp)

let  mapToCsvList xs = 
    List.fold (fun acc elem -> 
        match elem with 
        | Pass z ->  mapToCsvType(z)::acc
        | _ -> acc) [] xs

let myRows = 
   safeInput|> 
   List.ofSeq|>
   mapToCsvList

let myCsv = new MyCsvType(myRows)

let csvStrig = myCsv.SaveToString()
Console.WriteLine(csvStrig)

let wr = new System.IO.StreamWriter("Csv.csv")
wr.Write csvStrig
wr.Close()