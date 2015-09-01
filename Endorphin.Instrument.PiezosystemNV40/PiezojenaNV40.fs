namespace Endorphin.Instrument.PiezosystemNV40

// 103538, NV40 serial number.

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40
open FSharp.Control.Reactive

module PiezojenaNV40 = 

    [<AutoOpen>]
    module private Logger =

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
    
    module Piezojena =      
        
        /// Retrieves Piezojena identification string. 
        let getIdentification piezojena = 
            let stage = id piezojena
            let getIdentificationWorkflow =
                async{
                let mutable identification : string = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving Piezojena's identification string."
                stage.GetIdentification(&identification)
                return identification 
                }
            getIdentificationWorkflow |> check piezojena  
        
        /// Retrieves Piezojena serial number. 
        let getSerialNumber piezojena (serial:string)= 
            let stage = id piezojena
            let getSerialNumberWorkflow = 
                async{
                let mutable serialNumber : int64 = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving Piezojena's serial number."
                stage.GetSerialNumber (&serialNumber)
                return serialNumber
                }
            getSerialNumberWorkflow |> check piezojena 

        /// Retrieves the Piezojena's software version, uses three pointers to major, minor and build.
        /// Version of form major.minor.build.
        let getVersion piezojena = 
            let stage = id piezojena         
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
            getVersionWorkflow |> check piezojena 
    
    
    module Display = 
        
        /// Sets econder values.
        let setEncoder piezojena (encoder:Encoder) =
            let stage = id piezojena         
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
                }
            setEncoderWorkflow |> check piezojena 

        /// Queries the encoders values. 
        let queryEncoder piezojena =     
            let stage = id piezojena 
            let queryEncoderWorkflow =
                async{  
                let mutable mode : Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode = Unchecked.defaultof<_>
                let mutable time : int           = Unchecked.defaultof<_>
                let mutable stepLimit : int      = Unchecked.defaultof<_>
                let mutable exponent : byte      = Unchecked.defaultof<_>
                let mutable closedStep : float32 = Unchecked.defaultof<_> 
                let mutable openStep : float32   = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving encoder values."
                stage.GetEncoder ( &mode , &time, &stepLimit, &exponent, &closedStep, &openStep)
                return (mode, time, stepLimit, exponent, closedStep, openStep)
                }
            queryEncoderWorkflow |> check piezojena  
            
        /// Sets display brigthness. 
        let setDisplayBrightness piezojena (brightness:byte) = 
            let stage = id piezojena 
            let setBrightnessWorkflow = 
                async{
                logDevice piezojena "Setting display brightness."
                stage.SetDisplayBrightness brightness 
                }   
            setBrightnessWorkflow |> check piezojena 

        /// Queries the display brightness. 
        let queryDisplayBrightness piezojena = 
            let stage = id piezojena 
            let queryBrightnessWorkflow = 
                async{
                let mutable brightness : byte = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving display brightness."
                stage.GetDisplayBrightness (&brightness)
                return brightness
                }
            queryBrightnessWorkflow |> check piezojena 


    
    module SetModes = 

        /// Sets closed loop on and off, if the mode boolean is true then closed lopp, if false then open loop. 
        let private setLoopMode piezojena (channel:Channel) (mode:Loop) = 
            let stage = id piezojena
            let setLoopModeWorkflow =         
                async{
                let byteChannel = Parsing.channelByte channel 
                let modeBoolean = Parsing.loopBoolean mode
                logDevice piezojena "Setting loop mode."
                stage.SetClosedLoopControlled (byteChannel, modeBoolean)
                }
            setLoopModeWorkflow |> check piezojena  

        /// Sets all channels to same loop mode. 
        let setLoopModeAllChannels piezojena (mode:Loop) = 
            let stage = id piezojena 
            let modeBoolean = Parsing.loopBoolean mode
            logDevice piezojena "Setting loop mode for all channels."
            let setLoopZeroWorkflow = 
                async{
                logDevice piezojena "Setting channel 0 loop mode."
                stage.SetClosedLoopControlled (0uy, modeBoolean)
                }
            let setLoopOneWorkflow =
                async{
                logDevice piezojena "Setting channel 1 loop mode."
                stage.SetClosedLoopControlled (1uy, modeBoolean)
                } 
            let setLoopTwoWorkflow =
                async{
                logDevice piezojena "Setting channel 2 loop mode."
                stage.SetClosedLoopControlled (2uy, modeBoolean)
                }
            let workflowArray = [|setLoopZeroWorkflow; setLoopOneWorkflow; setLoopTwoWorkflow|]
            workflowArray |> checkMulti piezojena   


        /// Sets remote control for one channel. 
        let internal setRemoteControl piezojena (channel:Channel) (switch: Switch) = 
            let stage = id piezojena 
            let setRemoteControlWorkflow = 
                async{
                let byteChannel = Parsing.channelByte (channel)
                let booleanSwitch = Parsing.switchBoolean switch
                logDevice piezojena "Attempting to change remote control mode."
                stage.SetRemoteControlled (byteChannel, booleanSwitch)
                }
            setRemoteControlWorkflow |> check piezojena  

        /// Sets all three channels remote control modes on or off.
        let internal setAllRemoteControl piezojena (switch:Switch) = 
            let stage = id piezojena 
            let booleanSwitch = Parsing.switchBoolean switch
            logDevice piezojena "Attempting to change all channels remote control modes."
            let setRemoteZeroWorkflow = 
                async{
                logDevice piezojena "Setting remote mode for channel 0." 
                stage.SetRemoteControlled (0uy, booleanSwitch)
                }
            let setRemoteOneWorkflow  = 
                async{
                logDevice piezojena "Setting remote mode for channel 1."
                stage.SetRemoteControlled (1uy, booleanSwitch)
                }
            let setRemoteTwoWorkflow = 
                async{
                logDevice piezojena "Setting remote mode for channel 2."
                stage.SetRemoteControlled (2uy, booleanSwitch)
                }
            let workflowArray = [|setRemoteZeroWorkflow; setRemoteOneWorkflow; setRemoteTwoWorkflow|]
            workflowArray |> checkMulti piezojena 
                          
        /// Sets soft start initalisation and then sleeps for 10 seconds. 
        let setSoftStart piezojena (softstart:Switch) = 
            let setSoftStartWorkflow =     
                async{
                let stage = id piezojena 
                let boolean = Parsing.switchBoolean(softstart)
                logDevice piezojena "Changing soft start settings."
                stage.SetSoftStart (boolean)
                Async.Sleep 10000 |> Async.RunSynchronously
                }
            setSoftStartWorkflow |> check piezojena  

    module Query = 

        /// Queries the actuators temperature. 
        let queryTemperature piezojena =     
            let stage = id piezojena 
            let queryTemperatureWorkflow = 
                async{ 
                let mutable temperature : float32 = Unchecked.defaultof<_>
                logDevice piezojena "Measureing temperature."            
                stage.GetTemperature (&temperature)
                return temperature
                }
            queryTemperatureWorkflow |> check piezojena 
        
        /// Queries the actuator posistion for a single channel. 
        let private queryAcuatorCoordinate piezojena (channel:Channel) =     
            let stage = id piezojena
            let queryAcuatorCoordinateWorkflow= 
                async{ 
                let byteChannel = Parsing.channelByte (channel)
                let mutable coordinate : Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving channel coordinates."
                stage.GetActuatorCoordinate (byteChannel , &coordinate)
                return coordinate 
                }
            queryAcuatorCoordinateWorkflow |> check piezojena  

        /// Queries closed loop  limits. 
        let queryClosedLoopLimits piezojena (channel:Channel) = 
            let stage = id piezojena 
            let queryClosedLoopLimitsWorkflow  = 
                async{
                let byteChannel = Parsing.channelByte (channel)
                let mutable minimum : float32 = Unchecked.defaultof<_>
                let mutable maximum : float32 = Unchecked.defaultof<_>
                logDevice piezojena "Retrieving closed loop limits."
                stage.GetClosedLoopLimits (byteChannel, &minimum , &maximum)
                return (minimum, maximum)
                }
            queryClosedLoopLimitsWorkflow |> check piezojena 
       
        /// Queries a measurement from a single channel. 
        let private queryChannelPosition piezojena (channel:Channel) = 
            let stage = id piezojena 
            let queryChannelPositionWorkflow = 
                async{
                let mutable position : float32 = Unchecked.defaultof<_>  
                let byteChannel = Parsing.channelByte (channel)
                logDevice piezojena "Checking channel position."   
                stage.GetMeasuredValue (byteChannel , &position)
                return position
                }
            queryChannelPositionWorkflow |> check piezojena 
        
    
    module Initialise = 
        
        /// Sets all channels to closed loop with remote control and the stage posistion to the origin.
        let initialise portName  = asyncChoice{
            let piezojena = SerialConnection.Standard portName 
            let stage = id piezojena 
            do! SetModes.setAllRemoteControl piezojena On             
            do! SetModes.setLoopModeAllChannels piezojena ClosedLoop 
            return piezojena   
            }

        let close piezojena = 
            (id piezojena).Dispose()

    module Motion = 
        
        let PositionSet = new Event<float32*float32*float32>()
       
        /// Queries all measurements from three channels. 
        let queryPosition piezojena = 
            let stage = id piezojena 
            let queryAllPositionsWorkflow = 
                async{ 
                let mutable array = [|Unchecked.defaultof<_>; Unchecked.defaultof<_>; Unchecked.defaultof<_>|]
                logDevice piezojena "Checking all channel measurements." 
                stage.GetMeasuredValueChunk (&array)
                let x = Array.get array 0
                let y = Array.get array 1
                let z = Array.get array 2
                return (x, y, z)
                }
            queryAllPositionsWorkflow |> check piezojena 

        /// Sets the output of a single channel.
        let private setOutput piezojena (channel:Channel) (output:float32) = 
            let stage = id piezojena 
            let setOutputWorkflow = 
                async{    
                let byteChannel = Parsing.channelByte (channel)
                logDevice piezojena "Setting channel output."
                stage.SetDesiredOutput (byteChannel, output)
                }
            setOutputWorkflow |> check piezojena

        /// Sets all channel outputs. 
        let private setAllOutputs piezojena (x:float, y:float, z:float) resolution =   
            let stage  = id piezojena
            let target = IntensityMap.Tuples.typetoFloat32 (x, y, z)
            let setAllOutputsWorkflow  =         
                async{
                logDevice piezojena "Setting outputs of all channels."
                Parsing.tupletoArray target |> stage.SetDesiredOutputChunk  
                }
            
            setAllOutputsWorkflow |> check piezojena  
        
        /// Checks if position had been reached. 
        let rec private waitToReachPosition piezojena count ((x:float, y:float, z:float)) (tolerancefloat:float) = asyncChoice{      
            let target = IntensityMap.Tuples.typetoFloat32 (x, y, z)
            let tolerance = float32 (tolerancefloat)
            if count > 10 then 
                return! (fail "Failed to reach position")
            else     
                let! coordinate = queryPosition piezojena 
                let positionReached = IntensityMap.Tuples.compare target coordinate tolerance
                if positionReached then
                    return coordinate 
                else 
                    do! Async.Sleep 5 |> AsyncChoice.liftAsync 
                    return! waitToReachPosition piezojena (count + 1) (x, y, z) tolerancefloat 
        }

        /// Sets all channel outputs using a 3 element tuple and then checks if in correct posistion. 
        let setPosition piezojena (target:float*float*float) tolerance = asyncChoice{
            let newtolerance = float32 tolerance 
            // Async workflow for setting all outputs. 
            do! setAllOutputs piezojena target newtolerance
            // Recursive function for querying position
            return! waitToReachPosition piezojena 0 target tolerance
            }     

        /// Sets all channel outputs using a 2 element tuple and a starting position.  
        let setGridPosition piezojena (first:Channel) (second:Channel) (target:float*float) start tolerance = asyncChoice{
            let newtolerance = float32 tolerance 
            let fullCoordinate = IntensityMap.Coordinates.arrangeCoordinate first second target start 
            // Async workflow for setting all outputs. 
            do! setAllOutputs piezojena fullCoordinate newtolerance
            // Recursive function for querying position
            return! waitToReachPosition piezojena 0 fullCoordinate tolerance
            }     
    
        /// Gnerates a grid of points for intensity mapping using current position. 
        let generateGrid piezojena  (firstAxis:Axis) (secondAxis:Axis) (interval:float) = asyncChoice{
            let! start = queryPosition piezojena
            let arrayofPoints = IntensityMap.GridPoints.generate firstAxis secondAxis interval start
            return arrayofPoints 
            }