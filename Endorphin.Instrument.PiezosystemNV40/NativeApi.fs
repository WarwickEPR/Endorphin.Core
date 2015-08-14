namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core
open System.Text
open System.Runtime.InteropServices

module NativeApi = 

    module internal NativeApi =
        [<Literal>]
        let dllName = "Piezojena.Protocols.Nv40Multi.dll" 
    
        [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetIdentification")>]
        /// Retrieves the Piezojena's identification string.
        extern Error GetIdentification(StringBuilder)

        [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetSerialNumber")>]
        /// Retrieves the Piezojena's identification string.
        extern Error GetSerialNumber([<Out>] int)

        [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetIdentification")>]
        /// Retrieves the Piezojena's identification string.
        extern Error GetIdentification(StringBuilder)
