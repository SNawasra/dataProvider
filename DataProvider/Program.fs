module Data

open System
open FSharp.Data
open Chessie.ErrorHandling
open Econnect 
open Microsoft.Dynamics.GP.eConnect;
open Microsoft.Dynamics.GP.eConnect.Serialization;
open System.Collections.Generic
open System.IO
open System.Xml;
open System.Xml.Serialization;
open System.Text;

let checkLength len (str:string) = 
    if(str.Length > len ) then 
        let s = String.Format("exeeded max length {0}", len)
        fail s //<|String15 s
    else 
        pass <| str

let ValidateStringLength (len:int) =  checkLength len

// open the csv file 
let csv = new CsvProvider<"data.csv">()


// an api to print the result 
let PrintResults (x:Sample') =
    printfn "%s, %A, %s" (x.BatchNumber.ToString()) ( (x.SourceDocument)) (x.JERef.ToString())

// read data from csv, and this data unsafe. we will apply some rules on to be safe
let unsafeInput =
    csv.Rows
    |> Seq.map(fun x -> {Batch=x.Batch; Date=x.Date;AccountNumber=x.AccountNumber;``JE Reference``=x.``JE Reference``;``Discribution Reference``=x.``Distribution Reference``;``DIVISION AA CODE``=x.``DIVISION AA CODE``; ``PROFIT CENTER AA CODE``=x.``PROFIT CENTER AA CODE``; Project= x.PROJECT; Deal=x.DEAL;Amount=x.Amount} )

let seqJERef = unsafeInput |>Seq.map(fun x-> x.``JE Reference``)
let JERefdist = Seq.toList (Seq.distinct seqJERef)

let validateDiscripRef = stringEmptyNullFail >> bind (ValidateStringLength 30)
let DiscRefValidation (inp) = validateDiscripRef inp

let valdRef = JERefdist |> List.map(fun item -> DiscRefValidation (item)) 
//let valdRef' = valdRef |> Seq.filter(fun item -> isSuccess item )

let  validInput xs = 
    List.fold (fun acc elem -> 
        match elem with 
        | Pass z ->  
            printfn("%A sss") elem
            z::acc
        | _ -> acc) [] xs


let valdRef' = 
    valdRef|> 
    List.ofSeq|>
    validInput


let groups = List<List<InpSchema>>()
for JEREF in valdRef' do
    let tt = new List<InpSchema>()
    unsafeInput |> Seq.iter(fun item -> if(JEREF = item.``JE Reference``) then tt.Add(item))
    groups.Add(tt)

let validateBatch  =  stringEmptyNullWarn >> bind (ValidateStringLength 15)
let BatchValidaton (inp:InpSchema) = validateBatch inp.Batch
let validateAccountNumber str = lift Some ((ValidateStringLength 11 str))


let inputValidation (x:InpSchema) =
    trial {
        let! listRes =
            //[BatchValidaton; DiscRefValidation]
            [BatchValidaton]
            |>List.map(fun f -> f x)
            |>collect

        return {BatchNumber=x.Batch;SourceDocument= option.None;``Discribution Reference``=x.``Discribution Reference`` ;AccountNumber= x.AccountNumber;JERef=x.``Discribution Reference``;Date=x.Date;``DIVISION AA CODE``=x.``DIVISION AA CODE``;``PROFIT CENTER AA CODE``=x.``PROFIT CENTER AA CODE``; Project=x.Project; Deal=x.Deal; Amount=x.Amount}  
    }

let validation (order: List<InpSchema>) = 
    let seqOrder = order |>List.ofSeq
    let validOrder = seqOrder |> Seq.map(fun x -> inputValidation x )
    let validOrder' = List<Sample'>(validOrder|> List.ofSeq|>validInput)
    let connectionString = "temp"
    let jeEntry = DocumentBuilder connectionString
    //let header = SetTransactionHeader jeEntry validOrder'.[0]
    let temp = InsertTransaction validOrder' jeEntry connectionString
    //let lines = SetTransactionLine jeEntry validOrder'
    order

//let valSring15 = lift Some (ValidateStringLength 11)
// Rules to apply on the data that we read from the csv file
//let validateBatch  =  stringEmptyNullWarn >> bind (ValidateStringLength 15)
//let BatchValidaton (inp:InpSchema) = validateBatch inp.Batch
//let validateAccountNumber str = lift Some ((ValidateStringLength 11 str))
//let validateDiscripRef = stringEmptyNullFail >> bind (ValidateStringLength 30)
//let DiscRefValidation (inp:InpSchema) = validateDiscripRef inp.``Discribution Reference``

// error handlers

    
// apply the error handlers on the unsafe data to get safe data
let safeInput =
    unsafeInput 
    |> Seq.map(fun x -> inputValidation x )



let safeInput'' = 
    safeInput|> 
    List.ofSeq|>
    validInput

//type Sample' = {BatchNumber:string;SourceDocument:string option; ``Discribution Reference``:string;AccountNumber:string;JERef:string; Date:DateTime; ``DIVISION AA CODE``:string;``PROFIT CENTER AA CODE``:string; Project:string; Deal:string; Amount:decimal}



// 
//type MyCsvType = CsvProvider<
//                Sample="BatchNumber, SourceDocument, JERef", 
//                Schema = "BatchNumber (string), SourceDocument (string), JERef (string)", 
//                HasHeaders = true>
//
//// add result on csv row 
//let mapToCsvType x =
//    MyCsvType.Row(string x.BatchNumber, string x.JERef,"")
//
////let  mapToCsvList xs  =
////       printfn "got to start "
////       let rec loop xs acum=
////           match xs with 
////           | h::t -> 
////               printfn "this is the HEAD:  %A \r\n " h
////               printfn "this is the Tail:  %A \r\n " t 
////               match h with 
////               |Pass z ->                                        
////                   loop t ((mapToCsvType z)::acum)  
////               | _ -> loop t acum                
////           | [] ->                
////               acum
////       loop xs []                  
//
//
//// add all safe data to csv provider
//let  mapToCsvList xs = 
//    List.fold (fun acc elem -> 
//        match elem with 
//        | Pass z ->  
//            printfn("%A") elem
//            mapToCsvType(z)::acc
//        | _ -> acc) [] xs
//
//// all safed data 
//let myRows = 
//    safeInput|> 
//    List.ofSeq|>
//    mapToCsvList
//
//let myCsv = new MyCsvType(myRows)
//let csvStrig = myCsv.SaveToString()
//
//Console.WriteLine(csvStrig)
//
//// write the results on the created csv file
//let wr = new System.IO.StreamWriter("Csv.csv")
//wr.Write csvStrig
//wr.Close()