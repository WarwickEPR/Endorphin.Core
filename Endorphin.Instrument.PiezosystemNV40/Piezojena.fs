namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open ExtCore.Control.Choice
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40

module Piezojena = 
    
    /// Checks return value of the NativeApi function and converts to a success or gives an error message.
    let internal checkStatus = 
        function
        | Error.Ok -> succeed ()
        | Error.Error string -> fail string 

    let internal checkStatusAndReturn value status = choice {
        do! checkStatus status
        return value }

    /// Creates log for PicoHarp 300.
    let log = log4net.LogManager.GetLogger "PicoHarp 300"

    /// Logs the PicoHarp.
    let internal logDevice (piezojena : Piezojena) message =
        sprintf "[%A] %s" piezojena message |> log.Info

    /// Logs a success or failure message based on result of function. 
    let internal logQueryResult successMessageFunc failureMessageFunc input =
        match input with
        | Success value -> successMessageFunc value |> log.Debug
        | Failure error -> failureMessageFunc error |> log.Error
        input 
        
    /// Logs a success or failure message based on result of function using the PicoHarp's index.
    let internal logDeviceQueryResult (piezojena : Piezojena) successMessageFunc failureMessageFunc =
        logQueryResult 
            (fun value -> sprintf "[%A] %s" piezojena (successMessageFunc value))
            (fun error -> sprintf "[%A] %s" piezojena (failureMessageFunc error))

    let internal logDeviceOpResult picoHarp300 successMessage = logDeviceQueryResult picoHarp300 (fun _ -> successMessage)
    
    /// The device index. 
    let identification (Piezojena ID) = ID 
    
    module PiezojenaInformation= 
  

    let identificationString = 
        let identification = StringBuilder (8) 
        Piezojena.Protocols.Nv40Multi.Nv40MultiCommon GetIdentification (identification)
