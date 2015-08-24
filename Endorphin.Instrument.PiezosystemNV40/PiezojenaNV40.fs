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
    
    /// Connects to serial port COM3. 
    let private multiServices = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
    let stage = multiServices.ConnectNv40MultiToSerialPort("COM3")    
    
    /// Handles errors, if no errors returns function values else fails. 
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
    
    /// Runs workflow and returns a status.
    let private stausCheck (workflow:Async<'T>) = 
        workflow |> Async.RunSynchronously |> ignore 
        let mutable error : string = Unchecked.defaultof<_> 
        stage.GetCommandError (&error)
        stringtoStatus error 
        

    /// Takes argument of an array of async workflows and returns a list of thier status codes. 
    let private statusList (workflowArray : Async<'T> [])  = 
        let length = (Array.length workflowArray) - 1
        let list = []
        let rec results element errorList =
            if element > 0 || element =  0 then 
                let workflow = Array.get workflowArray element
                let listElement  = stausCheck workflow  
                let list = listElement :: errorList 
                results (element - 1) list  
            else 
                errorList  
        results length list 
    
    /// Filters Ok messages from list, if list length is zero then there are no errors and Ok is returned, else the
    /// first error from the list is returned. 
    let private filterList list = 
        let filteredList = List.filter (fun elem -> elem = Ok) list 
        let length = List.length filteredList 
        if length = 0 then 
            Ok
        else 
            List.item 0 list  

    let checkMulti (workFlowArray : Async<'T>[]) = workFlowArray |> statusList |> filterList   

    [<AutoOpen>]
    module private Logger =

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
            let getIdentificationWorkflow =
                async{        
                let mutable identification : string = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving Piezojena's identification string."
                stage.GetIdentification(&identification)
                return identification 
                }
            getIdentificationWorkflow |> check
        
        /// Retrieves Piezojena serial number. 
        let getSerialNumber piezojena (serial:string)= 
            let getSerialNumberWorkflow = 
                async{
                let mutable serialNumber : int64 = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving Piezojena's serial number."
                stage.GetSerialNumber (&serialNumber)
                return serialNumber
                }
            getSerialNumberWorkflow |> check

        /// Retrieves the Piezojena's software version, uses three pointers to major, minor and build.
        /// Version of form major.minor.build.
        let getVersion piezojena = 
            let getVersionWorkflow =         
                async{        
                let mutable major : int    = Unchecked.defaultof<_>
                let mutable minor : int    = Unchecked.defaultof<_>
                let mutable build : int    = Unchecked.defaultof<_>
                let mutable time : System.DateTime = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving software version.."
                stage.GetVersion (&major, &minor, &build, &time)
                return (major, minor, build, time)
                }
            getVersionWorkflow |> check 

    module SetParameters = 

        /// Sets closed loop on and off, if the mode boolean is true then closed lopp, if false then open loop. 
        let setLoopMode piezojena (channel:Channel) (mode:Loop) = 
            let setLoopModeWorkflow =         
                async{
                let byteChannel = Parsing.channelByte channel 
                let modeBoolean = Parsing.loopBoolean mode
                logDevice piezojena "Setting loop mode."
                stage.SetClosedLoopControlled (byteChannel, modeBoolean)
                return ()
                }
            setLoopModeWorkflow |> check 

        /// Sets all channels to same loop mode. 
        let setLoopModeallChannels piezojena (mode:Loop) = 
            logDevice piezojena "Setting loop mode for all channels."
            let modeBoolean = Parsing.loopBoolean mode
            let setLoopZeroWorkflow = 
                async{
                logDevice piezojena "Setting channel 0 loop mode."
                stage.SetClosedLoopControlled (0uy, modeBoolean)
                return ()
                }
            let setLoopOneWorkflow =
                async{
                logDevice piezojena "Setting channel 1 loop mode."
                stage.SetClosedLoopControlled (1uy, modeBoolean)
                return ()
                } 
            let setLoopTwoWorkflow =
                async{
                logDevice piezojena "Setting channel 2 loop mode."
                stage.SetClosedLoopControlled (2uy, modeBoolean)
                return ()
                }
            let workflowArray = [|setLoopZeroWorkflow; setLoopOneWorkflow; setLoopTwoWorkflow|]
            workflowArray |> checkMulti  


        /// Sets remote control for one channel. 
        let setRemoteControl piezojena (channel:Channel) (switch: Switch) = 
            let setRemoteControlWorkflow = 
                async{
                let byteChannel = Parsing.channelByte (channel)
                let booleanSwitch = Parsing.switchBoolean switch
                logDevice piezojena "Attempting to change remote control mode."
                stage.SetRemoteControlled (byteChannel, booleanSwitch)
                return ()
                }
            setRemoteControlWorkflow |> check 

        /// Sets all three channels remote control modes on or off.
        let setAllRemoteControl piezojena (switch:Switch) = 
            logDevice piezojena "Attempting to change all channels remote control modes."
            let booleanSwitch = Parsing.switchBoolean switch
            let setRemoteZeroWorkflow = 
                async{
                logDevice piezojena "Setting remote mode for channel 0." 
                stage.SetRemoteControlled (0uy, booleanSwitch)
                return ()
                }
            let setRemoteOneWorkflow  = 
                async{
                logDevice piezojena "Setting remote mode for channel 1."
                stage.SetRemoteControlled (1uy, booleanSwitch)
                return ()
                }
            let setRemoteTwoWorkflow = 
                async{
                logDevice piezojena "Setting remote mode for channel 2."
                stage.SetRemoteControlled (2uy, booleanSwitch)
                return ()
                }
            let workflowArray = [|setRemoteZeroWorkflow; setRemoteOneWorkflow; setRemoteTwoWorkflow|]
            workflowArray |> checkMulti
                

        /// Sets econder values.
        let setEncoder piezojena (encoder:Encoder) =
            let setEncoderWorkflow = 
                async{        
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
                return ()
                }
            setEncoderWorkflow |> check 
        
        /// Sets soft start, if true then soft start turned on, if false then off. 
        let setSoftStart piezojena (softstart:Switch) = 
            let setSoftStartWorkflow =     
                async{
                let boolean = Parsing.switchBoolean(softstart)
                logDevice piezojena "Changing soft start settings."
                stage.SetSoftStart (boolean)
                return ()
                }
            setSoftStartWorkflow |> check 
        
        /// Sets the output of a single channel.
        let setOutput piezojena (channel:Channel) (output:float32) = 
            let setOutputWorkflow = 
                async{    
                let byteChannel = Parsing.channelByte (channel)
                logDevice piezojena "Setting channel output."
                stage.SetDesiredOutput (byteChannel, output)
                return ()
                }
            setOutputWorkflow |> check 
       
        /// Sets all channel outputs. 
        let setAllOutputs piezojena (outputTuple:float*float*float) =   
            let setAllOutputsWorkflow =         
                async{
                logDevice piezojena "Setting outputs of all channels."
                Parsing.tupletoArray outputTuple |> stage.SetDesiredOutputChunk  
                return ()
                }
            setAllOutputsWorkflow |> check 

    module Query = 

        /// Queries the encoders values. 
        let queryEncoder piezojena =     
            let queryEncoderWorkflow =
                async{  
                let mutable mode : Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode = Unchecked.defaultof<_>
                let mutable time : int         = Unchecked.defaultof<_>
                let mutable stepLimit : int    = Unchecked.defaultof<_>
                let mutable exponent : byte    = Unchecked.defaultof<_>
                let mutable closedStep : float32 = Unchecked.defaultof<_> 
                let mutable openStep : float32   = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving encoder values."
                stage.GetEncoder ( &mode , &time, &stepLimit, &exponent, &closedStep, &openStep)
                return (mode, time, stepLimit, exponent, closedStep, openStep)
                }
            queryEncoderWorkflow |> check  

        /// Queries the actuators temperature. 
        let queryTemperature piezojena =     
            let queryTemperatureWorkflow = 
                async{ 
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
                let mutable position : float32 = Unchecked.defaultof<_>  
                let byteChannel = Parsing.channelByte (channel)
                logDevice piezojena "Checking channel position."   
                stage.GetMeasuredValue (byteChannel , &position)
                return position
                }
            queryChannelPositionWorkflow |> check 
        
        /// Retrieves all measurements from three channels. 
        let queryAllPositionsl piezojena = 
            let queryAllPositionsWorkflow = 
                async{ 
                let mutable array = [|Unchecked.defaultof<_>; Unchecked.defaultof<_>; Unchecked.defaultof<_>|]
                logDevice piezojena "Checking all channel measurements." 
                stage.GetMeasuredValueChunk (&array)
                return array
                }
            queryAllPositionsWorkflow |> check 
        
