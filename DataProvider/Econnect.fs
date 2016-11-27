module Econnect 
    open System
    open Microsoft.Dynamics.GP.eConnect;
    open Microsoft.Dynamics.GP.eConnect.Serialization;
    open Chessie.ErrorHandling
    open Microsoft.Dynamics.GP.eConnect;
    open Microsoft.Dynamics.GP.eConnect.Serialization;

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


//    let DocumentBuilder connectionString = 
//        let docNumbers = new GetNextDocNumbers()
//        let jeEntry =  Convert.ToInt32(docNumbers.GetNextGLJournalEntryNumber(IncrementDecrement.Increment, connectionString))
//        jeEntry
//
//    type setLineRes = {lineTr:taGLTransactionLineInsert_ItemsTaGLTransactionLineInsert;aaItemsTr:List<taAnalyticsDistribution_ItemsTaAnalyticsDistribution>}
//

    (*https://msdn.microsoft.com/en-us/library/jj193353.aspx*)
    type GLTrxHeader = {BatchNumber:String15;JournalEntry:Int4;Reference:String30;TransactionDate:DateTime;ReversingDate:DateTime option ;TransactionType:TransactionType;SequenceLine:int option; Series:Series option;CurrencyID:String15 option;ExchangeRate:int option;
        RateTypeId:String15 option; ExpirationDate:DateTime option; ExchangeDate:DateTime option; RateExpiration:RateExpiration option ; DaysToIncrement:Int2 option;RateVariance: int option; TransactionDateDefault: TrxDefaultDate option;
        RateCalculationMethod:RateCalculationMethod option; PerviousDayLimits:Int2 option; DateLimits:DateLimits option; Time1:DateTime option ; RequestTransaction:ReqTrx;SourceDocument:String11 option;LedgerID:LedgerID option ;
        UserId:String15 option; AdjustmentTransaction:AdjTrx option; NoteText:String8k option}


    let SetTransactionHeader conString = 
        let docNumber = new GetNextDocNumbers()
        let je = docNumber.GetNextGLJournalEntryNumber(IncrementDecrement.Increment,conString )
        let s = new taGLTransactionHeaderInsert()
        ()
