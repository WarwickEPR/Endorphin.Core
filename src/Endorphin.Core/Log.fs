// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

module Endorphin.Core.Log

open log4net

/// A dummy type so we have something local to reflect onto to get the namespace string.
type private Dummy = Dummy
/// A string containing the current namespace.
// We reflect onto it rather than using a string literal so we always get it right, even
// if we decide to change the namespace in the future.
let private ns = typeof<Dummy>.Namespace

/// Create the logging repository if required, else just do nothing.
let private createRepoIfNeeded str =
    let repos = LogManager.GetAllRepositories ()
    if Array.exists (fun (x : Repository.ILoggerRepository) -> x.Name = ns) repos then ()
    else LogManager.CreateRepository ns |> ignore

/// Get a logger in the namespace's own repository for the specified type or class.
let typed<'T> =
    createRepoIfNeeded ns
    LogManager.GetLogger (ns, typeof<'T>)

/// Get a logger in the namespace's own repository with the specified name.
let named (str : string) =
    createRepoIfNeeded ns
    LogManager.GetLogger (ns, str)

/// Generic to perform the string construction and pass it on to the relevant continuation.
/// The format of the id message may be changed here.
let private generic cont id = Printf.ksprintf (sprintf "%s: %s" id >> cont)

/// Issue a formatted DEBUG message on the given log.
let debugf (log : ILog) id = generic log.Debug id
/// Issue a formatted INFO message on the given log.
let infof  (log : ILog) id = generic log.Info  id
/// Issue a formatted WARN message on the given log.
let warnf  (log : ILog) id = generic log.Warn  id
/// Issue a formatted ERROR message on the given log.
let errorf (log : ILog) id = generic log.Error id
/// Issue a formatted FATAL message on the given log.
let fatalf (log : ILog) id = generic log.Fatal id

/// Issue a DEBUG message on the given log.
let debug log id = debugf log id "%s"
/// Issue an INFO message on the given log.
let info  log id = infof  log id "%s"
/// Issue a WARN message on the given log.
let warn  log id = warnf  log id "%s"
/// Issue an ERROR message on the given log.
let error log id = errorf log id "%s"
/// Issue a FATAL message on the given log.
let fatal log id = fatalf log id "%s"
