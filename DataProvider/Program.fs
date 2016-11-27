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

// input schema
// type InpSchema = {Batch:string;Date:DateTime;AccountNumber:string;``JE Reference``:string;``Discribution Reference``:string; }
type InpSchema = {Batch:string;Date:DateTime;AccountNumber:string;``JE Reference``:string;``Discribution Reference``:string;``DIVISION AA CODE``:string;``PROFIT CENTER AA CODE``:string; Project:string; Deal:string; Amount:decimal }

// output schema
//type Sample = {BatchNumber:String15;SourceDocument:String11 option;JERef:String30 }
//type Sample' = {BatchNumber:string;SourceDocument:string option;JERef:string }
type Sample' = {BatchNumber:string;SourceDocument:string option; ``Discribution Reference``:string;AccountNumber:string;JERef:string; Date:DateTime; ``DIVISION AA CODE``:string;``PROFIT CENTER AA CODE``:string; Project:string; Deal:string; Amount:decimal}

// an api to print the result 
let PrintResults x =
    printfn "%s, %A, %s" (x.BatchNumber.ToString()) ( (x.SourceDocument)) (x.JERef.ToString())

// read data from csv, and this data unsafe. we will apply some rules on to be safe
let unsafeInput =
    csv.Rows
    |> Seq.map(fun x -> {Batch=x.Batch; Date=x.Date;AccountNumber=x.AccountNumber;``JE Reference``=x.``JE Reference``;``Discribution Reference``=x.``Distribution Reference``;``DIVISION AA CODE``=x.``DIVISION AA CODE``; ``PROFIT CENTER AA CODE``=x.``PROFIT CENTER AA CODE``; Project= x.PROJECT; Deal=x.DEAL;Amount=x.Amount} )

//let valSring15 = lift Some (ValidateStringLength 11)
// Rules to apply on the data that we read from the csv file
let validateBatch  =  stringEmptyNullWarn >> bind (ValidateStringLength 15)
let BatchValidaton (inp:InpSchema) = validateBatch inp.Batch
let validateAccountNumber str = lift Some ((ValidateStringLength 11 str))
let validateDiscripRef = stringEmptyNullFail >> bind (ValidateStringLength 30)
let DiscRefValidation (inp:InpSchema) = validateDiscripRef inp.``Discribution Reference``

// error handlers
let inputValidation (x:InpSchema) =
    trial {
        let! listRes =
            [BatchValidaton; DiscRefValidation]
            |>List.map(fun f -> f x)
            |>collect

        return {BatchNumber=x.Batch;SourceDocument= option.None;``Discribution Reference``=x.``Discribution Reference`` ;AccountNumber= x.AccountNumber;JERef=x.``Discribution Reference``;Date=x.Date;``DIVISION AA CODE``=x.``DIVISION AA CODE``;``PROFIT CENTER AA CODE``=x.``PROFIT CENTER AA CODE``; Project=x.Project; Deal=x.Deal; Amount=x.Amount}  
    }
    
// apply the error handlers on the unsafe data to get safe data
let safeInput =
    unsafeInput 
    |> Seq.map(fun x -> inputValidation x )
//
let DocumentBuilder connectionString = 
    let docNumbers = new GetNextDocNumbers()
    let jeEntry =  Convert.ToInt32(docNumbers.GetNextGLJournalEntryNumber(IncrementDecrement.Increment, connectionString))
    jeEntry
 
//let CreateJE docNumberBuilder JE_Data = 
//    let docNum = DocumentBuilder
//    docNum
//
//let CreateJE' = CreateJE DocumentBuilder
//let CreateJE jE_Data = CreateJE' jE_Data

type setLineRes = {lineTr:taGLTransactionLineInsert_ItemsTaGLTransactionLineInsert;aaItemsTr:List<taAnalyticsDistribution_ItemsTaAnalyticsDistribution>}

let SetTransactionHeader jeEntry (inp:Sample')=
    let header = new  taGLTransactionHeaderInsert()
//    let docNumbers = new GetNextDocNumbers()
//    let jeEntry =  Convert.ToInt32(docNumbers.GetNextGLJournalEntryNumber(IncrementDecrement.Increment, connectionString))
    header.JRNENTRY <- jeEntry;
    header.BACHNUMB <- inp.BatchNumber
    header.TRXDATE <- inp.Date.ToString()
    header.REFRENCE <- inp.JERef
    header.TRXTYPE <- 1s
    let AccountNumber = inp.AccountNumber.PadLeft(7, '0')
    let Entity = AccountNumber.Substring(0, 2)                       
    header.USRDEFND1 <- Entity;
    header.USRDEFND2 <- "0";
    header

let SetTransactionLine jeEntry (row:Sample') (lineSequence) = 
    //let lineSequence = 0
    //lineSequence = (lineSequence + 16384)
    let line = new taGLTransactionLineInsert_ItemsTaGLTransactionLineInsert()
    line.BACHNUMB <- row.BatchNumber
    line.JRNENTRY <- jeEntry;
    let amount = System.Math.Round(row.Amount,2)
    let total = row.Amount ;

    if (amount < 0m) then
        line.CRDTAMNT <- System.Math.Round(amount * (-1m),2);
    else
        line.DEBITAMT <- System.Math.Round(amount,2)

    let AccountNumber = row.AccountNumber.PadLeft(7, '0') 
    let Entity = AccountNumber.Substring(0, 2)                       
    let NaturalAccount = AccountNumber.Substring(2, 5);
    let tmp = Entity + "-" + NaturalAccount
    line.ACTNUMST <- tmp;
    line.SQNCLINE <- lineSequence;
    line.ORTRXDESC <- row.``Discribution Reference``//distribution refrence
    line.DSCRIPTN <- row.``Discribution Reference``
    let aaItems = new List<taAnalyticsDistribution_ItemsTaAnalyticsDistribution>()

    if(not (String.IsNullOrEmpty(row.``DIVISION AA CODE``)))then 
        
        let aaAsset = new taAnalyticsDistribution_ItemsTaAnalyticsDistribution();

        aaAsset.ACTNUMST <- line.ACTNUMST;
        aaAsset.DistRef <- row.``Discribution Reference``
        aaAsset.DistSequenceSpecified <- true;
        aaAsset.DistSequence <- int lineSequence;
        aaAsset.aaTrxDim <- "Division";
        if(row.``DIVISION AA CODE``.Length < 3) then 
            aaAsset.aaTrxDimCode <- row.``DIVISION AA CODE``.PadLeft(3, '0')
        else
            aaAsset.aaTrxDimCode <- row.``DIVISION AA CODE``
    
        aaAsset.DOCNMBR <- string jeEntry;
        aaAsset.DOCTYPE <- 0s;
        aaItems.Add(aaAsset)

    if(not (String.IsNullOrEmpty(row.``PROFIT CENTER AA CODE``)))then 
        let aaAsset = new taAnalyticsDistribution_ItemsTaAnalyticsDistribution();
        aaAsset.ACTNUMST <- line.ACTNUMST;
        aaAsset.DistRef <- row.``Discribution Reference``
        aaAsset.DistSequenceSpecified <- true;
        aaAsset.DistSequence <- int lineSequence;
        aaAsset.aaTrxDim <- "Profit Center";
        if(row.``PROFIT CENTER AA CODE``.Length < 3) then 
            aaAsset.aaTrxDimCode <- row.``PROFIT CENTER AA CODE``.PadLeft(3, '0')
        else
            aaAsset.aaTrxDimCode <- row.``PROFIT CENTER AA CODE``

        aaAsset.DOCNMBR <- string jeEntry;
        aaAsset.DOCTYPE <- 0s;
        aaItems.Add(aaAsset)

    if(not (String.IsNullOrEmpty(row.Project)))then 
        let aaAsset = new taAnalyticsDistribution_ItemsTaAnalyticsDistribution();
        aaAsset.ACTNUMST <- line.ACTNUMST;
        aaAsset.DistRef <- row.``Discribution Reference``
        aaAsset.DistSequenceSpecified <- true;
        aaAsset.DistSequence <- int lineSequence;
        aaAsset.aaTrxDim <- "Project";
        if(row.Project.Length < 3) then 
            aaAsset.aaTrxDimCode <- row.Project.PadLeft(3, '0')
        else
            aaAsset.aaTrxDimCode <- row.Project

        aaAsset.DOCNMBR <- string jeEntry;
        aaAsset.DOCTYPE <- 0s;
        aaItems.Add(aaAsset)

    if(not (String.IsNullOrEmpty(row.Deal)))then 
        let aaAsset = new taAnalyticsDistribution_ItemsTaAnalyticsDistribution();
        aaAsset.ACTNUMST <- line.ACTNUMST;
        aaAsset.DistRef <- row.``Discribution Reference``
        aaAsset.DistSequenceSpecified <- true;
        aaAsset.DistSequence <- int lineSequence;
        aaAsset.aaTrxDim <- "Deal";
        if(row.Deal.Length < 3) then 
            aaAsset.aaTrxDimCode <- row.Deal.PadLeft(3, '0')
        else
            aaAsset.aaTrxDimCode <- row.Deal

        aaAsset.DOCNMBR <- string jeEntry;
        aaAsset.DOCTYPE <- 0s;
        aaItems.Add(aaAsset)

    {lineTr=line;aaItemsTr=aaItems}


let InsertTransaction (inp:seq<Result<Sample',string>>) = 
    let eConnect = new eConnectType()
    let glArray = new List<GLTransactionType>()
    //let glArray = new GLTransactionType[1]
    let glType = new GLTransactionType()
    let  procInfo = new eConnectProcessInfo()
    procInfo.ProcTimeOut <- "10000";
    glType.eConnectProcessInfo <- procInfo;

    let connectionString = "connectionString"
    let JeEntry = DocumentBuilder connectionString
    //let firstRow = fst(inp.[0])
    //let header = SetTransactionHeader JeEntry inp    
    //glType.taGLTransactionHeaderInsert <- header;////

    //glType.taGLTransactionLineInsert_Items = lines.ToArray();///
    //glType.taAnalyticsDistribution_Items = aaItems.ToArray();////
    
    glArray.Add(glType)
    eConnect.GLTransactionType <- glArray.ToArray();
//    if XMLSerialize("JE.xml", eConnect) then 
//        if (eConnectEntry("JE.xml")) then 
                   
    inp

//let safeInput' = safeInput |> Seq.iter(fun x -> (PrintResults <!> x) |> ignore  )


let XMLSerialize  (fileName:string) (eConnect:eConnectType) =

    try 

        let fs = new FileStream(fileName, FileMode.Create);
        let writer = new XmlTextWriter(fs, new UTF8Encoding());
        let serializer = new XmlSerializer(eConnect.GetType());
        serializer.Serialize(writer, eConnect);
        writer.Close();               
        true
    with 
        _ -> false

let eConnectEntry (connectionString:string)(fileName:string)=
    let message = false
    let eConnect = new eConnectMethods()
    let myXmlDoc = new XmlDocument()
    //let eConnectProcessInfoOutgoing

    try
        try 
            myXmlDoc.Load(fileName);
            let eConnectProcessInfoOutgoing = myXmlDoc.SelectSingleNode("//Outgoing");
            eConnect.CreateTransactionEntity(connectionString, myXmlDoc.OuterXml);
            message = true;

        with
            _ -> message = false 

    finally 
        eConnect.Dispose()
 
 
type MyCsvType = CsvProvider<
                Sample="BatchNumber, SourceDocument, JERef", 
                Schema = "BatchNumber (string), SourceDocument (string), JERef (string)", 
                HasHeaders = true>

// add result on csv row 
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


// add all safe data to csv provider
let  mapToCsvList xs = 
    List.fold (fun acc elem -> 
        match elem with 
        | Pass z ->  
            printfn("%A") elem
            mapToCsvType(z)::acc
        | _ -> acc) [] xs

// all safed data 
let myRows = 
    safeInput|> 
    List.ofSeq|>
    mapToCsvList

let myCsv = new MyCsvType(myRows)
let csvStrig = myCsv.SaveToString()

Console.WriteLine(csvStrig)

// write the results on the created csv file
let wr = new System.IO.StreamWriter("Csv.csv")
wr.Write csvStrig
wr.Close()