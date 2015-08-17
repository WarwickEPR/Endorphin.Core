namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open ExtCore.Control.Choice
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40

module PiezojenaNV40 = 
    
    [<AutoOpen>]
    module private Logger =
        
        /// Returns Ok or Error with the error string. 
        let stringtoStatus = function
            | "OK, No Error." -> Ok
            | str -> Error str
        
        /// Checks return value of the NativeApi function and converts to a success or gives an error message.
        let checkStatus = function
            | Ok            -> succeed ()
            | Error message -> fail message

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
        
        /// The device identification string. 
        let identification (Piezojena ID) = ID 
            
    module PiezojenaInformation = 
        
        /// Retrieves Piezojena identification string. 
        let getIdentification piezojena = 
            let errorString = StringBuilder (8)
            let identification = StringBuilder (8)
            logDevice piezojena "Retrieving Piezojena's identification string."
            NativeApi.GetIdentification (identification)
            NativeApi.GetCommandError (errorString)
            let error = string(errorString)
            error
            |> stringtoStatus 
            |> checkStatus 
            |> logDeviceOpResult piezojena 
                ("Successfully retrieved the Piezojena's identification string.")
                (sprintf "Failed to retrieve the Piezojena's identification string: %A ")
            |> AsyncChoice.liftChoice 
        
        /// Retrieves Piezojena serial number. 
        let getSerialNumber piezojena = 
            let errorString = StringBuilder (8)
            let mutable serial : int = Unchecked.defaultof<_>
            logDevice piezojena "Retrieving Piezojena's serial number."
            NativeApi.GetSerialNumber (&serial)
            NativeApi.GetCommandError (errorString)
            let error = string (errorString)
            error
            |> stringtoStatus 
            |> checkStatus 
            |> logDeviceOpResult piezojena 
                ("Successfully retrieved the Piezojena's serial number.")
                (sprintf "Failed to retrieve the Piezojena's serial number: %A ")
            |> AsyncChoice.liftChoice 

        /// Retrieves the Piezojena's software version, uses three pointers to major, minor and build.
        /// Version of form major.minor.build.
        let getVersion piezojena = 
            let errorString = StringBuilder (8)
            let mutable major : int = Unchecked.defaultof<_>
            let mutable minor : int = Unchecked.defaultof<_>
            let mutable build : int = Unchecked.defaultof<_>
            let mutable time : System.DateTime = Unchecked.defaultof<_>
            NativeApi.GetVersion (&major, &minor, &build, &time)
            let error = string (NativeApi.GetCommandError (errorString))
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully retrieved software version.")
                (sprintf "Failed to retrieve software version: %A")
            |> AsyncChoice.liftChoice
    
    module SetParameters = 
        
        /// Changes channel, requires the Piezojena's ID.
        let changeChannel piezojena (channel:Channel) = 
            let errorString = StringBuilder (8)
            let byteChannel = Parsing.channelByte channel 
            let id = identification piezojena
            NativeApi.ChangeChannel (id, byteChannel)
            NativeApi.GetCommandError (errorString)
            let error = string(errorString)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully changed channel.")
                (sprintf "Failed to change channel: %A")
            |> AsyncChoice.liftChoice

        /// Sets closed loop on and off, if the mode boolean is true then closed lopp, if false then open loop. 
        let setLoopMode piezojena (channel:Channel) (mode:Loop) = 
            let errorString = StringBuilder (8)
            let byteChannel = Parsing.channelByte channel 
            let modeBoolean = Parsing.loopBoolean mode
            NativeApi.SetClosedLoopControlled (byteChannel, modeBoolean)
            NativeApi.GetCommandError (errorString)
            let error = string(errorString)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena 
                ("Successfully set loop mode.")
                (sprintf "Failed to set loop mode: %A")
            |> AsyncChoice.liftChoice
    
    module Query = 

        let queryEncoder piezojena =  
            let errorString = StringBuilder (8)
            let mutable mode : Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode = Unchecked.defaultof<_>
            let mutable time : int         = Unchecked.defaultof<_>
            let mutable stepLimit : int    = Unchecked.defaultof<_>
            let mutable exponent : byte    = Unchecked.defaultof<_>
            let mutable closedStep : float = Unchecked.defaultof<_> 
            let mutable openStep : float   = Unchecked.defaultof<_>
            NativeApi.GetEncoder ( &mode , &time, &stepLimit, &exponent, &closedStep, &openStep)
            NativeApi.GetCommandError (errorString)
            let error = string(errorString)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena 
                ("Successfully retrieved encoder values.")
                (sprintf "Failed to retrieve encoder values: %A")
            |> AsyncChoice.liftChoice

        /// Queries the actuators temperature. 
        let queryTemperature piezojena =  
            let errorString = StringBuilder (8)
            let mutable temperature : float = Unchecked.defaultof<_>
            NativeApi.GetTemperature (&temperature)
            NativeApi.GetCommandError (errorString)
            let error = string(errorString)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully measured actuator temperature.")
                (sprintf "Failed to measure actuator temperature: %A")
            |> AsyncChoice.liftChoice
            extern void GetEncoder([<Out>] Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode mode, [<Out>] int& timeMilliseconds, [<Out>] int& steplimit, [<Out>] byte& exponent, [<Out>] float& closedstep, [<Out>] float& openstep )    

    module EncoderScan = 

        // let openDevice picoHarp300 = 
        //    let serial = StringBuilder (8)
        //    logDevice picoHarp300 "Opening device."
        //    NativeApi.OpenDevice (index picoHarp300, serial) 
        //    |> checkStatus 
        //    |> logDeviceOpResult picoHarp300 
        //        ("Successfully opened the PicoHarp.")
        //        (sprintf "Failed to open the PicoHarp: %A.")
        //    |> AsyncChoice.liftChoice
        