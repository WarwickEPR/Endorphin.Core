// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Experiment.HighFieldEpr

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Media
open System.Windows.Threading
open FsXaml

type MainWindow = XAML<"MainWindow.xaml">

type MainWindowController() =
    inherit WindowViewController<MainWindow>()

    override x.OnLoaded window =
        let ctx = window.Root.DataContext :?> CwEprViewModel
        window.Root.Closing
        |> Event.add (fun args ->
            match ctx.Connection with
            | Connected _ -> 
                "Cannot close window while instruments are connected."
                |> MessageBox.Show |> ignore
                args.Cancel <- true
            | _ -> ())
