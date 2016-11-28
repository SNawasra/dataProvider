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

// string length validation
let checkLength len (str:string) = 
    if(str.Length > len ) then 
        let s = String.Format("exeeded max length {0}", len)
        fail s //<|String15 s
    else 
        pass <| str

// partial function for length alidation
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

// get all JERef to distinct them
let seqJERef = unsafeInput |>Seq.map(fun x-> x.``JE Reference``)
let JERefdist = Seq.toList (Seq.distinct seqJERef)

// validate JERef
let validateDiscripRef = stringEmptyNullFail >> bind (ValidateStringLength 30)
let DiscRefValidation (inp) = validateDiscripRef inp

// valid JERef
let valdRef = JERefdist |> List.map(fun item -> DiscRefValidation (item)) 
//let valdRef' = valdRef |> Seq.filter(fun item -> isSuccess item )

let  validInput xs = 
    List.fold (fun acc elem -> 
        match elem with 
        | Pass z ->  
            printfn("%A sss") elem
            z::acc
        | _ -> acc) [] xs


// filter the JERef
let valdRef' = valdRef|> List.ofSeq|> validInput

// group the input schema by valid JERef
let groups = List<List<InpSchema>>()
for JEREF in valdRef' do
    let tt = new List<InpSchema>()
    unsafeInput |> Seq.iter(fun item -> if(JEREF = item.``JE Reference``) then tt.Add(item))
    groups.Add(tt)

// input validation
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

// insert valid input 
let InsertValidInput (order: List<InpSchema>) = 
    let seqOrder = order |>List.ofSeq
    let validOrder = seqOrder |> Seq.map(fun x -> inputValidation x )
    let validOrder' = List<Sample'>(validOrder|> List.ofSeq|>validInput)
    let connectionString = "temp"

    let mutable success = 0m
    let mutable failure = 0m 
    let mutable sucFail = {success=0m;failure=0m;error=0m}

    if(validOrder'.Count = Seq.length(validOrder)) then 
       let jeEntry = DocumentBuilder connectionString
       sucFail <- (InsertTransaction validOrder' jeEntry connectionString)
       ignore()
    else 
       ignore()

    sucFail
    //let lines = SetTransactionLine jeEntry validOrder'

let mutable success = 0m 
let mutable failure = 0m
let mutable error = 0m

for order in groups do 
    let errorLog = InsertValidInput order
    success <- errorLog.success
    failure <- errorLog.failure
    error <- errorLog.error
    //ignore() 
    