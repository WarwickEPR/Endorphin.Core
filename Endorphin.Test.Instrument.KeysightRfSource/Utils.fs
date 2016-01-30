// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight

open System
open log4net
open log4net.Config
open Endorphin.Core.NationalInstruments
open Endorphin.Instrument.Keysight.RfSource

module Utils =

    let configureLogToFile (prefix : string) (path : string) =
        let logconfig = """
    <log4net>
    <appender name="FileAppender" type="log4net.Appender.FileAppender">
      <file value="_FILE_" />
      <appendToFile value="false" />
      <layout type="log4net.Layout.PatternLayout">
        <conversionPattern value="%date [%thread] %-5level %logger [%property{NDC}] - %message%newline" />
      </layout>
    </appender>
    <root>
      <level value="DEBUG" />
      <appender-ref ref="FileAppender" />
    </root>
    </log4net>
    """

        let logconfigxml = new Xml.XmlDocument()
        logconfig.Replace("_FILE_",path) |> logconfigxml.LoadXml 
        XmlConfigurator.Configure(logconfigxml.DocumentElement) |> ignore
        log4net.LogManager.GetLogger (prefix)
    
    let mockVI (response : string) (cmdtest : string -> unit) =
        { new VisaInstrument with
          member vi.Query command = cmdtest(command); async { return response }
          member vi.Write command = cmdtest(command)
          member vi.Read () = async { return response }
          member vi.Close () = async { return () } }

    let mockRfI (response : string) (cmdtest : string -> unit) =
        openRfInstrument <| mockVI response cmdtest

    let queryMock (response : string) (cmdtest : string -> unit) =
        mockRfI response cmdtest

    let writeMock (cmdtest : string -> unit) =
        mockRfI "" cmdtest

    let readMock (response : string) =
        mockRfI response (fun _ -> ())

    let run (a : Async<'T>) =
        Async.RunSynchronously a
