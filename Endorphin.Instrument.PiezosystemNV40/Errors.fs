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
    let internal checkStatus = function
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
        let multiStatusCheck (workflowArray : Async<'T> [])  = 
            let length = (Array.length workflowArray) - 1
            let rec results index  =
                if index > 0 || index =  0 then 
                    let workflow = Array.get workflowArray index
                    let status  = statusCheck workflow  
                    if status = Ok then
                        results (index - 1)   
                    else failwithf "%A" status
                else 
                    Ok 
            ()
       
        workFlowArray |> multiStatusCheck  

