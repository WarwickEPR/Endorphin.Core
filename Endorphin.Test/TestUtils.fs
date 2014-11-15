module TestUtils

open NUnit.Framework
open log4net.Config

type Assert with
    static member ArrayElementsAreEqual(expected, actual) =
        let arraysAreEqual =
            if Array.length expected <> Array.length actual then false
            else Array.forall2 (fun elem1 elem2 -> elem1 = elem2) expected actual

        Assert.IsTrue(arraysAreEqual, (sprintf "Expected: %A\n  But was: %A" expected actual))

type log4netConfig() =
    static do BasicConfigurator.Configure() |> ignore
