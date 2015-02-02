namespace Endorphin.Core.CSharpInterop

open System
open System.Reactive.Linq
open System.Runtime.CompilerServices
open System.Windows.Forms

[<Extension>]
type WinFormsExtensions() =

    /// Helper extension method giving an observable sequence for a WinForms Control Click event.
    [<Extension>]
    static member GetClickObservable (control : Control) =
        Observable.FromEventPattern<EventArgs>(control, "Click")
