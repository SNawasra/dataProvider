module Econnect 
    open System
    open Microsoft.Dynamics.GP.eConnect;
    open Microsoft.Dynamics.GP.eConnect.Serialization;
    open Chessie.ErrorHandling
    open Microsoft.Dynamics.GP.eConnect;
    open Microsoft.Dynamics.GP.eConnect.Serialization;
    open Microsoft.Dynamics.GP.eConnect;
    open Microsoft.Dynamics.GP.eConnect.Serialization;
    open System.Collections.Generic
    open System.IO
    open System.Xml;
    open System.Xml.Serialization;
    open System.Text;


    type Series = All|Financial|Sales|Purchasing|Inventory|Payroll|Project
    let getSeries= function
        | All -> 1
        | Financial -> 2
        | Sales -> 3
        | Purchasing ->4
        | Inventory ->5
        | Payroll -> 6
        | Project -> 7

    type TransactionType = Regular|Reversing    
    let GetTransactionType= function
        | Regular ->1
        | Reversing -> 2


    type RateExpiration= None|Daily|Weekly|``Bi-Weekly``|Semiweekly|Monthly|Quarterly|Annually|Miscellaneous|None9
    let GetRateExpiration = function 
        | None -> 0
        | Daily -> 1
        | Weekly -> 2
        | ``Bi-Weekly`` -> 3
        | Semiweekly -> 4
        | Monthly -> 5
        | Quarterly -> 6
        | Annually -> 7
        | Miscellaneous ->8
        | None9 -> 9


    type TrxDefaultDate = ExactDate|NextDate|PreviousDate
    let GetTrxDefaultDate = function
        | ExactDate -> 0
        | NextDate -> 1
        | PreviousDate -> 2

    type RateCalculationMethod = Multiple|Divide
    let GetRateCalculationMethod = function 
        | Multiple->1
        | Divide ->2


    type DateLimits = Unlimited|Limited
    let GetDateLimits = function
        | Unlimited-> 0
        | Limited -> 1

    type ReqTrx = bool
    let GetReqTrx = function
        | false-> 0
        | true -> 1


    type LedgerID = Base|IFRS|Local
    let GetLedgerID = function
        | Base-> 1
        | IFRS -> 2
        | Local -> 3

    type AdjTrx  = bool
    let GetAdjTrx= function
        | false-> 0
        | true -> 1

    type String15 = String15 of string
    let createString15 s = 
        if String.IsNullOrEmpty(s) then
            warn "Null or empty string" <|String15 s
        elif String.length s > 15 then
            fail "exeeded max length 15" //<|String15 s
        else
           String15 s
           |>
            pass 


    type String30 = String30 of string
    let createString30 s  = 
        if String.IsNullOrEmpty(s) then
            fail "Null or empty string"
        elif String.length s > 30 then
            fail "exeeded max length 30" 
        else
           pass <| String30 s

    type String11 = String11 of string
    let createString11 s = 
        if String.IsNullOrEmpty(s) then        
            fail "null or empty string"  
        elif String.length s > 11 then
            fail  "exeeded max length of 11" 
        else
           String11 s
           |>
            pass 

        
    type String8k = String8k of string
    let createString8k s = 
        if String.IsNullOrEmpty(s) then
            Option.None 
        elif String.length s > 8000 then
            Option.None 
        else
           Some (String8k s)



    type Int4 = Int4 of int
    let createInt4 (s:int)  = 
        if  s.ToString().Length > 4 then
            fail "exeeded length of 4"
        else
           Int4 s |> pass

    type Int2 = Int2 of int
    let createInt2 (s:int)  = 
        if  s.ToString().Length > 2 then
            Option.None 
        else
           Some (Int2 s)


///// custom strings type
//type String15 = 
//    |String15 of string   
//    override this.ToString () = 
//       match this with 
//        | String15 (x) -> sprintf "%s" x
//    
//type String30 =
//    |String30 of string   
//    override this.ToString () = 
//       match this with 
//        | String30 (x) -> sprintf "%s" x
//
//type String11 = 
//    |String11 of string   
//    override this.ToString () = 
//       match this with 
//        | String11 (x) -> sprintf "%s" x

// Some rules that we can apply on the unsafe inputs


    // Rule to check if the string  is less or equal 15
    let StringMax15 s = 
        if String.length s > 15 then
            fail "exeeded max length 15" //<|String15 s
        else
            pass <| String15 s

    // Rule to check if the string  is less or equal 11
    let StringMax11 s = 
        if String.length s > 11 then
            fail "exeeded max length 15" //<|String15 s
        else
            pass <| String11 s

    // Rule to check if the string  is less or equal 30
    let StringMax30 s = 
        if String.length s > 30 then
            fail "exeeded max length 15" //<|String15 s
        else
            pass <| String30 s

    // Check empty or null and return string or warn message
    let stringEmptyNullFail s = 
        if String.IsNullOrEmpty(s) then
            warn "Null or empty string" <| s
        else
            s|> pass 
    // Check empty or null and return string or warn message
    let stringEmptyNullWarn s = 
        if String.IsNullOrEmpty(s) then
            warn "Null or empty string" <| s
        else
            s|> pass 

    // input schema
    // type InpSchema = {Batch:string;Date:DateTime;AccountNumber:string;``JE Reference``:string;``Discribution Reference``:string; }
    type InpSchema = {Batch:string;Date:DateTime;AccountNumber:string;``JE Reference``:string;``Discribution Reference``:string;``DIVISION AA CODE``:string;``PROFIT CENTER AA CODE``:string; Project:string; Deal:string; Amount:decimal }

    // output schema
    //type Sample = {BatchNumber:String15;SourceDocument:String11 option;JERef:String30 }
    //type Sample' = {BatchNumber:string;SourceDocument:string option;JERef:string }
    type Sample' = {BatchNumber:string;SourceDocument:string option; ``Discribution Reference``:string;AccountNumber:string;JERef:string; Date:DateTime; ``DIVISION AA CODE``:string;``PROFIT CENTER AA CODE``:string; Project:string; Deal:string; Amount:decimal}


    (*https://msdn.microsoft.com/en-us/library/jj193353.aspx*)
    type GLTrxHeader = {BatchNumber:String15;JournalEntry:Int4;Reference:String30;TransactionDate:DateTime;ReversingDate:DateTime option ;TransactionType:TransactionType;SequenceLine:int option; Series:Series option;CurrencyID:String15 option;ExchangeRate:int option;
        RateTypeId:String15 option; ExpirationDate:DateTime option; ExchangeDate:DateTime option; RateExpiration:RateExpiration option ; DaysToIncrement:Int2 option;RateVariance: int option; TransactionDateDefault: TrxDefaultDate option;
        RateCalculationMethod:RateCalculationMethod option; PerviousDayLimits:Int2 option; DateLimits:DateLimits option; Time1:DateTime option ; RequestTransaction:ReqTrx;SourceDocument:String11 option;LedgerID:LedgerID option ;
        UserId:String15 option; AdjustmentTransaction:AdjTrx option; NoteText:String8k option}

    type setLineRes = {linesTr:List<taGLTransactionLineInsert_ItemsTaGLTransactionLineInsert>;aaItemsTr:List<taAnalyticsDistribution_ItemsTaAnalyticsDistribution>}
    
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

    let SetTransactionLine jeEntry (rows: (Sample' List)) = 
        let mutable lineSequence = 0m
        let aaItems = new List<taAnalyticsDistribution_ItemsTaAnalyticsDistribution>()
        let lines = new List<taGLTransactionLineInsert_ItemsTaGLTransactionLineInsert>()

        //lineSequence = (lineSequence + 16384)
        for row  in rows do
            lineSequence <- (lineSequence + 16384m)
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

            lines.Add(line)

        {linesTr=lines;aaItemsTr=aaItems}



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

    type sucessFailureErr = {success:decimal;failure:decimal;error:decimal}

    let eConnectEntry (connectionString:string)(fileName:string)=
        let message = false
        let eConnect = new eConnectMethods()
        let myXmlDoc = new XmlDocument()
        //let eConnectProcessInfoOutgoing
        let mutable errorLog = {success=0m;failure=0m;error=0m}

        try
            try 
                myXmlDoc.Load(fileName);
                let eConnectProcessInfoOutgoing = myXmlDoc.SelectSingleNode("//Outgoing");
                eConnect.CreateTransactionEntity(connectionString, myXmlDoc.OuterXml);
                message = true;
                errorLog.failure = errorLog.failure + 1m

            with
                _ -> message = false 

        finally 
            eConnect.Dispose()


    let InsertTransaction (rows:List<Sample'>) jeEntry connectionString  = 
        let eConnect = new eConnectType()
        let glArray = new List<GLTransactionType>()
        //let glArray = new GLTransactionType[1]
        let glType = new GLTransactionType()
        let  procInfo = new eConnectProcessInfo()
        procInfo.ProcTimeOut <- "10000";
        glType.eConnectProcessInfo <- procInfo;


//        let connectionString = "connectionString"
//        let JeEntry = DocumentBuilder connectionString
        let temp = SetTransactionLine jeEntry rows

        //let firstRow = fst(inp.[0])
        let header = SetTransactionHeader jeEntry rows.[0]    
        glType.taGLTransactionHeaderInsert <- header;

        glType.taGLTransactionLineInsert_Items <- temp.linesTr.ToArray();///
        glType.taAnalyticsDistribution_Items <- temp.aaItemsTr.ToArray();////
    
        glArray.Add(glType)
        eConnect.GLTransactionType <- glArray.ToArray();

        let mutable success = 0m
        let mutable failure = 0m 
        let mutable error = 0m 


        if (XMLSerialize "JE.xml" eConnect) then 
            if (eConnectEntry connectionString "JE.xml") then 
                success <- success + 1m //success++
                ignore()//failure++           

            else 
                failure <- failure + 1m
                ignore()//failure++     
                      
        {success=success; failure=failure;error=0m}

//    
//    let SetTransactionHeader conString = 
//        let docNumber = new GetNextDocNumbers()
//        let je = docNumber.GetNextGLJournalEntryNumber(IncrementDecrement.Increment,conString )
//        let s = new taGLTransactionHeaderInsert()
//        ()
