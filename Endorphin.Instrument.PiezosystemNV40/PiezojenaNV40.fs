namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40

module PiezojenaNV40 = 
    
    let piezojena = Piezojena ""

    [<AutoOpen>]
    module private Logger =
        
        /// Returns Ok or Error with the error string. 
        let stringtoStatus = function
            | "OK, No Error." -> Ok
            | str -> Error str
        
        /// Converts string from status.
        let statustoString = function
            | Ok        -> "Ok, No Error."
            | Error str -> str

        /// Checks return value of the NativeApi function and converts to a success or gives an error message.
        let checkStatus = function
            | Ok            -> succeed ()
            | Error message -> fail message

        /// Deals with multiple errors. 
        let internal multipleErrors (errorArray:string[]) = 
           // Converts string array into status array. 
           let statusArray = Array.map (fun elem -> stringtoStatus elem) errorArray
           /// Filters out Ok's from status array.   
           let filterArray = Array.filter (fun elem -> elem <> Ok) statusArray
           let length = Array.length filterArray
           if Array.length filterArray = 0 then 
               Ok     
           else 
               let error = statustoString(Array.get filterArray 0) 
               Error error 

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
    
    let private multiServices = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
    let stage = multiServices.ConnectNv40MultiToSerialPort("COM3")    

    let private check (workflow:Async<'T>) = asyncChoice {
        let! workflowResult = workflow |> AsyncChoice.liftAsync
        let mutable error : string = Unchecked.defaultof<_> 
        stage.GetCommandError (&error)
        let statusError = stringtoStatus error
        do! if statusError = Ok then
                Ok |> checkStatus
            else 
                Error error |> checkStatus 
        return workflowResult } 

    module PiezojenaInformation =      
            
        /// Retrieves Piezojena identification string. 
        let getIdentification piezojena = 
            let mutable error : string = Unchecked.defaultof<_>
            let mutable identification : string = Unchecked.defaultof<_>
            logDevice piezojena "Retrieving Piezojena's identification string."
            stage.GetIdentification(&identification)
            stage.GetCommandError (&error)
            error
            |> stringtoStatus 
            |> checkStatus 
            |> logQueryResult  
                (sprintf "Successfully retrieved the Piezojena's identification string %A : %A" identification)
                (sprintf "Failed to retrieve the Piezojena's identification string, %s.")
            |> AsyncChoice.liftChoice 
        
        /// Retrieves Piezojena serial number. 
        let getSerialNumber piezojena (serial:string)= 
            let mutable error : string = Unchecked.defaultof<_>
            let mutable serialNumber : int64 = Unchecked.defaultof<_>
            logDevice piezojena "Retrieving Piezojena's serial number."
            stage.GetSerialNumber (&serialNumber)
            stage.GetCommandError (&error)
            error
            |> stringtoStatus 
            |> checkStatus 
            |> logQueryResult
                (sprintf "Successfully retrieved the Piezojena's serial number %A: %A" serialNumber)
                (sprintf "Failed to retrieve the Piezojena's serial number, %s. ")
            |> AsyncChoice.liftChoice 

        /// Retrieves the Piezojena's software version, uses three pointers to major, minor and build.
        /// Version of form major.minor.build.
        let getVersion piezojena = 
            let mutable error : string = Unchecked.defaultof<_>
            let mutable major : int    = Unchecked.defaultof<_>
            let mutable minor : int    = Unchecked.defaultof<_>
            let mutable build : int    = Unchecked.defaultof<_>
            let mutable time : System.DateTime = Unchecked.defaultof<_>
            logDevice piezojena "Retrieving software version.."
            stage.GetVersion (&major, &minor, &build, &time)
            stage.GetCommandError (&error)
            error
            |> stringtoStatus
            |> checkStatus
            |> logQueryResult
                (sprintf "Successfully retrieved software version %A: %A" (major, minor, build, time))
                (sprintf "Failed to retrieve software version, %s.")
            |> AsyncChoice.liftChoice
   
    module SetParameters = 

        /// Sets closed loop on and off, if the mode boolean is true then closed lopp, if false then open loop. 
        let setLoopMode piezojena (channel:Channel) (mode:Loop) = 
            let mutable error : string = Unchecked.defaultof<_>
            let byteChannel = Parsing.channelByte channel 
            let modeBoolean = Parsing.loopBoolean mode
            logDevice piezojena "Setting loop mode."
            stage.SetClosedLoopControlled (byteChannel, modeBoolean)
            stage.GetCommandError (&error)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena 
                ("Successfully set loop mode.")
                (sprintf "Failed to set loop mode, %s.")
            |> AsyncChoice.liftChoice
        
        /// Sets all channels to same loop mode. 
        let setLoopModeallChannels piezojena (mode:Loop) = 
            let mutable error1 : string = Unchecked.defaultof<_>
            let mutable error2 : string = Unchecked.defaultof<_>
            let mutable error3 : string = Unchecked.defaultof<_>
            let modeBoolean = Parsing.loopBoolean mode
            logDevice piezojena "Setting loop mode for all channels."
            stage.SetClosedLoopControlled (0uy, modeBoolean)
            stage.GetCommandError (&error1)
            stage.SetClosedLoopControlled (1uy, modeBoolean)
            stage.GetCommandError (&error2)
            stage.SetClosedLoopControlled (2uy, modeBoolean)
            stage.GetCommandError (&error3)
            let errorArray = [|error1; error2; error3|]
            multipleErrors errorArray
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully set the loop mode of all channels.")
                (sprintf "Failed to set the loop mode for all channels, %A")
            |> AsyncChoice.liftChoice 

        /// Sets remote control for one channel. 
        let setRemoteControl piezojena (channel:Channel) (switch: Switch) = 
            let mutable error : string = Unchecked.defaultof<_>
            let byteChannel = Parsing.channelByte (channel)
            let booleanSwitch = Parsing.switchBoolean switch
            logDevice piezojena "Attempting to change remote control mode."
            stage.SetRemoteControlled (byteChannel, booleanSwitch)
            stage.GetCommandError (&error)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully changed remote mode.")
                (sprintf "Failed to change remote mode: %A")
            |> AsyncChoice.liftChoice

        /// Sets all three channels remote control modes on or off.
        let setAllRemoteControl piezojena (switch:Switch) = 
            let mutable error1 : string = Unchecked.defaultof<_>
            let mutable error2 : string = Unchecked.defaultof<_>
            let mutable error3 : string = Unchecked.defaultof<_>
            let booleanSwitch = Parsing.switchBoolean switch
            logDevice piezojena "Attempting to change all channels remote control modes."
            stage.SetRemoteControlled (0uy, booleanSwitch)
            stage.GetCommandError (&error1)
            stage.SetRemoteControlled (1uy, booleanSwitch)
            stage.GetCommandError (&error2)
            stage.SetRemoteControlled (2uy, booleanSwitch)
            stage.GetCommandError (&error3)
            let errorArray = [|error1; error2; error3|]
            multipleErrors errorArray
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully changed all channels remote control modes.")
                (sprintf "Failed to change all channels remote control modes: %A")
            |> AsyncChoice.liftChoice

        /// Sets econder values.
        let setEncoder piezojena (encoder:Encoder) =
            let mutable error : string = Unchecked.defaultof<_>
            let mode       = Parsing.modeMap (encoder.Mode)
            let time       = encoder.Time
            let steplimit  = encoder.StepLimit
            let closedstep = encoder.ClosedStep
            let openstep   = encoder.OpenStep
            /// encoder.Exponent of type byte option, NativeApi function expects type byte.
            /// If Exponent is None then set exponent to 0uy, if Some byte then set to byte using byteOptionConvert. 
            let exponent   =     
                if encoder.Exponent = None then 0uy
                else Parsing.byteOptionConvert(encoder.Exponent)
            logDevice piezojena "Setting encoder values."            
            stage.SetEncoder (mode, time, steplimit, exponent, closedstep, openstep)
            stage.GetCommandError (&error)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully set the encoder values.")
                (sprintf "Failed to set the encoder values, %s.")
            |> AsyncChoice.liftChoice
        
        /// Sets soft start, if true then soft start turned on, if false then off. 
        let setSoftStart piezojena (softstart:Switch) = 
            let mutable error : string = Unchecked.defaultof<_>
            let boolean = Parsing.switchBoolean(softstart)
            logDevice piezojena "Changing soft start settings."
            stage.SetSoftStart (boolean)
            stage.GetCommandError (&error)
            error
            |> stringtoStatus 
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully changes soft start setting.")
                (sprintf "Failed to change soft start setting, %s.")
            |> AsyncChoice.liftChoice
        
        /// Sets the output of a single channel.
        let setOutput piezojena (channel:Channel) (output:float32) = 
            let mutable error : string = Unchecked.defaultof<_>
            let byteChannel = Parsing.channelByte (channel)
            logDevice piezojena "Setting channel output."
            stage.SetDesiredOutput (byteChannel, output)
            stage.GetCommandError (&error)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena 
                ("Successfully set the channel output.")
                (sprintf "Failed to set the channel output, %s.")
            |> AsyncChoice.liftChoice 
       
        /// Sets all channel outputs. 
        let setAllOutputs piezojena (outputTuple:float*float*float) =   
            let mutable error = Unchecked.defaultof<_>
            logDevice piezojena "Setting outputs of all channels."
            Parsing.tupletoArray outputTuple 
                |> stage.SetDesiredOutputChunk  
            stage.GetCommandError (&error)
            error
            |> stringtoStatus
            |> checkStatus
            |> logDeviceOpResult piezojena
                ("Successfully set the outputs of all channels.")
                (sprintf "Failed to set all channel outputs, %s.")
            |> AsyncChoice.liftChoice

    module Query = 

        /// Queries the encoders values. 
        let queryEncoder piezojena =     
            let queryEncoderWorkflow =
                async{  
                let mutable error : string = Unchecked.defaultof<_>
                let mutable mode : Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode = Unchecked.defaultof<_>
                let mutable time : int         = Unchecked.defaultof<_>
                let mutable stepLimit : int    = Unchecked.defaultof<_>
                let mutable exponent : byte    = Unchecked.defaultof<_>
                let mutable closedStep : float32 = Unchecked.defaultof<_> 
                let mutable openStep : float32   = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving encoder values."
                stage.GetEncoder ( &mode , &time, &stepLimit, &exponent, &closedStep, &openStep)
                }
            queryEncoderWorkflow |> check  

        /// Queries the actuators temperature. 
        let queryTemperature piezojena =     
            let queryTemperatureWorkflow = 
                async{ 
                let mutable error : string = Unchecked.defaultof<_>
                let mutable temperature : float32 = Unchecked.defaultof<_>
                logDevice piezojena "Measureing temperature."            
                stage.GetTemperature (&temperature)
                return temperature
                }
            queryTemperatureWorkflow |> check 
        
        /// Queries the actuator posistion for a single channel. 
        let queryAcuatorCoordinate piezojena (channel:Channel) =     
            let queryAcuatorCoordinateWorkflow= 
                async{ 
                let mutable error : string = Unchecked.defaultof<_>
                let byteChannel = Parsing.channelByte (channel)
                let mutable coordinate : Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving channel coordinates."
                stage.GetActuatorCoordinate (byteChannel , &coordinate)
                return coordinate 
                }
            queryAcuatorCoordinateWorkflow |> check 

        /// Queries closed loop  limits. 
        let queryClosedLoppLimits piezojena (channel:Channel) = 
            let queryClosedLoopLimitsWorkflow  = 
                async{
                let mutable error : string = Unchecked.defaultof<_>
                let byteChannel = Parsing.channelByte (channel)
                let mutable minimum : float32 = Unchecked.defaultof<_>
                let mutable maximum : float32 = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving closed loop limits."
                stage.GetClosedLoopLimits (byteChannel, &minimum , &maximum)
                return (minimum, maximum)
                }
            queryClosedLoopLimitsWorkflow |> check 
        
        /// Retrieves a measurement from a single channel. 
        let queryChannelPosition piezojena (channel:Channel) = 
            let queryChannelPositionWorkflow = 
                async{
                let mutable error : string = Unchecked.defaultof<_>
                let mutable position : float32 = Unchecked.defaultof<_>  
                let byteChannel = Parsing.channelByte (channel)
                logDevice piezojena "Checking channel position."   
                stage.GetMeasuredValue (byteChannel , &position)
                return position
                }
            queryChannelPositionWorkflow |> check 
        
        /// Retrieves all measurements from three channels. 
        let queryChannelPosition piezojena = 
            let queryAllPositionsWorkflow = 
                async{ 
                let mutable error : string = Unchecked.defaultof<_>
                let mutable array = [|Unchecked.defaultof<_>; Unchecked.defaultof<_>; Unchecked.defaultof<_>|]
                logDevice piezojena "Checking all channel measurements." 
                stage.GetMeasuredValueChunk (&array)
                return array
                }
            queryAllPositionsWorkflow |> check 
        
