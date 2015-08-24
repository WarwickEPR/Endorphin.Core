namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40
 
[<AutoOpen>]    
module Errors =
    
    /// Returns Ok or Error with the error string. 
    let private stringtoStatus = function
        | "OK, No Error." -> Ok
        | str -> Error str
    
    /// Converts string from status.
    let private statustoString = function
        | Ok        -> "Ok, No Error."
        | Error str -> str

    /// Checks return value of the NativeApi function and converts to a success or gives an error message.
    let private checkStatus = function
        | Ok            -> succeed ()
        | Error message -> fail message

    /// Handles errors, if no errors returns function values else fails. 
    let internal check piezojena (workflow:Async<'T>) = asyncChoice {
        
        let stage = id piezojena 
        
        let! workflowResult = workflow |> AsyncChoice.liftAsync
        let mutable error : string = Unchecked.defaultof<_>
        stage.GetCommandError (&error)
        let statusError = stringtoStatus error
        do! if statusError = Ok then
                Ok |> checkStatus
            else 
                Error error |> checkStatus
        return workflowResult }
    
        
    let internal checkMulti piezojena (workFlowArray : Async<'T>[]) =   
       
        let stage = id piezojena 

        /// Runs workflow and returns a status.
        let statusCheck (workflow:Async<'T>) = 
            workflow |> Async.RunSynchronously |> ignore 
            let mutable error : string = Unchecked.defaultof<_> 
            stage.GetCommandError (&error)
            stringtoStatus error 
        
        /// Takes argument of an array of async workflows and returns a list of thier status codes. 
        let statusList (workflowArray : Async<'T> [])  = 
            let length = (Array.length workflowArray) - 1
            let list = []
            let rec results index errorList =
                if index > 0 || index =  0 then 
                    let workflow = Array.get workflowArray index
                    let listElement  = statusCheck workflow  
                    let list = listElement :: errorList 
                    results (index - 1) list  
                else 
                    errorList  
            results length list 
        
        /// Filters Ok messages from list, if list length is zero then there are no errors and Ok is returned, else the
        /// first error from the list is returned. 
        let filterList list = 
            let filteredList = List.filter (fun elem -> elem = Ok) list 
            let length = List.length filteredList 
            if length = 0 then 
                Ok
            else 
                List.item 0 list 
       
        workFlowArray |> statusList |> filterList 

