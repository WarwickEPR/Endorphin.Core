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
    
    /// Module contains functions for logging.
    [<AutoOpen>]
    module internal Logger =

        /// Creates log for PicoHarp 300.
        let log = log4net.LogManager.GetLogger "PiezojenaNV40"

        /// Logs the PicoHarp.
        let logDevice (piezojena : Piezojena) message =
            sprintf "[%A] %s" piezojena message |> log.Info

        /// Logs a success or failure message based on result of function. 
        let logQueryResult successMessageFunc failureMessageFunc input =
            match input with
            | Success value -> successMessageFunc value |> log.Debug
            | Failure error -> failureMessageFunc error |> log.Error
            input 
            
        /// Logs a success or failure message based on result of function using the PicoHarp's index.
        let logDeviceQueryResult (piezojena : Piezojena) successMessageFunc failureMessageFunc =
            logQueryResult 
                (fun value -> sprintf "[%A] %s" piezojena (successMessageFunc value))
                (fun error -> sprintf "[%A] %s" piezojena (failureMessageFunc error))

        let logDeviceOpResult picoHarp300 successMessage = logDeviceQueryResult picoHarp300 (fun _ -> successMessage)

    [<AutoOpen>]
    module internal CheckErrros = 
       
        /// Returns Ok or Error with the error string. 
        let private stringtoStatus = function
            | "OK. No Error." -> Ok
            | str -> Error str
        
        /// Converts string from status.
        let private statustoString = function
            | Ok        -> "Ok. No Error."
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
            logDevice piezojena (error) 
            let statusError = stringtoStatus error
            do! if statusError = Ok then
                    Ok |> checkStatus
                else 
                    Error error |> checkStatus
            return workflowResult }
        
        /// Checks the errors for multiple async workflows.      
        let internal checkMulti piezojena (workflowArray : Async<'T>[]) =   
            let stage = id piezojena 
            // Runs workflow and returns a status.
            let statusCheck (workflow:Async<'T>) = asyncChoice {
                let! result = workflow |> AsyncChoice.liftAsync 
                let mutable error : string = Unchecked.defaultof<_> 
                stage.GetCommandError (&error)
                logDevice piezojena (error) 
                return stringtoStatus error
                }
              
            // Workflow takes an async workflow array and returns an asyncChoice. 
            let rec results index (workflowArray : Async<'T> []) = asyncChoice{
                  let length = Array.length workflowArray - 1 
                  if index < length || index =  length then 
                      let workflow = Array.get workflowArray index
                      let! status  = statusCheck workflow  
                      if status = Ok then
                        return! results (index + 1) workflowArray    
                      else 
                        return! (fail "Failed to execute all workflows.")
                  else 
                    return () 
            }
            results 0 workflowArray 
       
       
       
       
            
//        let internal checkMulti piezojena (workFlowArray : Async<'T>[]) =   
//            let stage = id piezojena 
//            /// Runs workflow and returns a status.
//            let statusCheck (workflow:Async<'T>) = 
//                workflow |> Async.RunSynchronously |> ignore 
//                let mutable error : string = Unchecked.defaultof<_> 
//                stage.GetCommandError (&error)
//                stringtoStatus error 
//            /// Takes argument of an array of async workflows and returns a list of thier status codes. 
//            let multiStatusCheck (workflowArray : Async<'T> [])  = 
//                let length = (Array.length workflowArray) - 1
//                let rec results index  =
//                    if index > 0 || index =  0 then 
//                        let workflow = Array.get workflowArray index
//                        let status  = statusCheck workflow  
//                        if status = Ok then
//                            results (index - 1)   
//                        else failwithf "%A" status
//                    else 
//                        Ok 
//                ()
//           
//            workFlowArray |> multiStatusCheck  