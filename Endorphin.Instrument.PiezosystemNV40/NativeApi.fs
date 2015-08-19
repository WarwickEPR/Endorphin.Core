namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core
open System.Text
open System.Runtime.InteropServices

module NativeApi = 
    
    module Protocols = 
        
        [<Literal>]
        let dllNameProtocols = "Piezojena.Protocols.dll"

        [<DllImport(dllNameProtocols, EntryPoint = "Configure")>]
        /// Configures connection using SerialConfigureation.
        extern void Configure (Piezojena.Protocols.SerialConfiguration configuration)

        [<DllImport(dllNameProtocols, EntryPoint = "SerialConfiguration")>]
        /// Configures connection using SerialConfiguration.
        extern void Configuration ()

        [<DllImport(dllNameProtocols, EntryPoint = "SerialPortConnection")>]
        /// Configures connection using SerialConfigureation.
        extern void SerialPortConnection (System.IO.Ports.SerialPort port)

    module Multi = 
    
        [<Literal>]
        let dllNameMulti = "Piezojena.Protocols.Nv40Multi.dll" 
        
        [<DllImport(dllNameMulti, EntryPoint = "GetCommandError")>]
        /// Retrieves error string for command. 
        extern void GetCommandError(StringBuilder)

        [<DllImport(dllNameMulti, EntryPoint = "GetIdentification")>]
        /// Retrieves the Piezojena's identification string.
        extern void GetIdentification(StringBuilder)
        
        [<DllImport(dllNameMulti, EntryPoint = "GetSerialNumber")>]
        /// Retrieves the Piezojena's serial number.
        extern void GetSerialNumber(int& serial)
        
        [<DllImport(dllNameMulti, EntryPoint = "GetVersion")>]
        /// Retrieves the Piezojena's version along with the time the version was retrieved.
        extern void GetVersion([<Out>] int& major, [<Out>] int& minor, [<Out>] int& build, [<Out>] System.DateTime& time )

        [<DllImport(dllNameMulti, EntryPoint = "Initialize")>]
        /// Retrieves the Piezojena's temperature in kelvin.
        extern void Initialise()
        
        [<DllImport(dllNameMulti, EntryPoint = "OnUpdateError")>]
        /// Retrieves the Piezojena's temperature in kelvin.
        extern void UpdateError (byte channel, Piezojena.Protocols.Nv40Multi.Nv40MultiErrors)

        [<DllImport(dllNameMulti, EntryPoint = "GetTemperature")>]
        /// Retrieves the Piezojena's temperature in kelvin.
        extern void GetTemperature([<Out>] float& temperature)
        
        [<DllImport(dllNameMulti, EntryPoint = "SetSoftStart")>]
        /// Sets soft start on or off. 
        extern void SetSoftStart(bool softstart)
        
        [<DllImport(dllNameMulti, EntryPoint = "SetClosedLoopControlled")>]
        /// If true then the channel set to closed loop control. Channel number is in the form of a byte. 
        extern void SetClosedLoopControlled(byte channel, bool closedloop)
        
        [<DllImport(dllNameMulti, EntryPoint = "GetClosedLoopLimits")>]
        /// Retrieves the closed loop limits.  
        extern void GetClosedLoopLimits(byte channel, [<Out>] float& minimum, [<Out>] float& maximum)
        
        [<DllImport(dllNameMulti, EntryPoint = "GetActuatorCoordinate")>]
        /// Retrieves the Piezojena's temperature in kelvin.
        extern void GetCoordinate(byte, [<Out>] Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate& coordinate )

        [<DllImport(dllNameMulti, EntryPoint = "ChangeChannel")>]
        /// Changes the channel, takes arguments ID as a string form and channel number as a byte.
        extern void ChangeChannel(string id, byte channel)
        
        [<DllImport(dllNameMulti, EntryPoint = "CheckChannel")>]
        // ?????????????????????????????????????
        extern void CheckChannel(byte channel)
        
        [<DllImport(dllNameMulti, EntryPoint = "SetDesiredOutput")>]
        /// Set desired output for a dingle channel.
        extern void SetOutput (byte channel, float output)
            
        [<DllImport(dllNameMulti, EntryPoint = "SetDesiredOutputChunk")>]
        // ???????????????????????????????????????
        extern void SetMultipleOutputs (float [])

        [<DllImport(dllNameMulti, EntryPoint = "GetEncoder")>]
        /// Retrieves experiemnt parameters, the mode, the time, the exponent used to calculate the acceleration,
        /// and the distance step in closed and open loop modes. 
        extern void GetEncoder([<Out>] Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode& mode, [<Out>] int& timeMilliseconds, [<Out>] int& steplimit, [<Out>] byte& exponent, [<Out>] float& closedstep, [<Out>] float& openstep )    

        [<DllImport(dllNameMulti, EntryPoint = "SetEncoder")>]
        /// Sets experiemnt parameters, the mode, the time, the exponent used to calculate the acceleration,
        /// and the distance step in closed and open loop modes. 
        extern void SetEncoder(Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode mode, int timeMilliseconds, int steplimit, byte exponent, float closedstep, float openstep )