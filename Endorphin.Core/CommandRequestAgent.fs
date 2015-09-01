namespace Endorphin.Core

open System
open ExtCore.Control
open log4net

[<AutoOpen>]
/// Generic command/request agent with error handling used to serialise posted commands and
/// requests which are defined by closures. Useful in serialising communications to an
/// instrument with a C API.
module CommandRequestAgent = 
    
    [<AutoOpen>]
    /// Command/request agent model.
    module Model =
        /// Agent message, defining a command or request. Each message case is parameteraised by
        /// a closure dependent on the state of the agent which is determined at initialisation. 
        /// Each command or request may fail, so the return type of the contained closures is of
        /// Choice<'Result, exn>. If the the failure case is returned, the exception is raised by
        /// the function which posts the command or request to the agent. Commands do not return a
        /// vaule in the success case, so the result type is unit. Requests can either return a
        /// value type or an object type. Command and request messages also include a description
        /// which is used for logging. Finally, the close message notifies the agent that it
        /// should stop processing messages.
        type internal CommandRequestMessage<'State> = 
            | Command       of description : string * commandFunc : ('State -> Choice<unit,      exn>) * replyChannel : AsyncReplyChannel<Choice<unit,      exn>>
            | ValueRequest  of description : string * requestFunc : ('State -> Choice<ValueType, exn>) * replyChannel : AsyncReplyChannel<Choice<ValueType, exn>>
            | ObjectRequest of description : string * requestFunc : ('State -> Choice<obj,       exn>) * replyChannel : AsyncReplyChannel<Choice<obj,       exn>>
            | Close         of                        closeFunc   : ('State -> Choice<unit,      exn>) * replyChannel : AsyncReplyChannel<Choice<unit,      exn>>
    
        /// A command/request agent processes messages of CommandRequestMessage type. The details
        /// of the implementation are hidden from the user of the library.
        type CommandRequestAgent<'State> = internal CommandRequestAgent of Agent<CommandRequestMessage<'State>>

    [<RequireQualifiedAccess>]
    /// Functions for creating and interacting with a generic command/request agent with error
    /// handling used to serialise posted commands and requests which are defined by closures.
    module CommandRequestAgent =

        /// Returns a string describing the message.
        let private messageDescription = function
            | Command       (description, _, _) 
            | ValueRequest  (description, _, _)
            | ObjectRequest (description, _, _) -> description
            | Close _                           -> "Close"

        /// Asynchronously starts a new command/request agent with the given name (function of
        /// state, used for logging purposes) and initialisation workflow. The initialisation
        /// may fail, in which case this workflow will return failure. Furthermore, the
        /// processing of any given message may fail.
        let create<'State> (nameFunc : 'State -> string) (init : unit -> Async<Choice<'State, exn>>) = async { 
            let! initResult = init () // perform initialisation
            match initResult with
            | Failure exn   -> raise exn ; return Unchecked.defaultof<_>
            | Success state ->
                return CommandRequestAgent // if it succeeds, start the mailbox processing loop
                <| Agent.Start(fun (mailbox : Agent<CommandRequestMessage<'State>>) ->
                    let log = LogManager.GetLogger (nameFunc state) // create a logger
                
                    let logResponse description = function
                        | Success s           -> sprintf "Successfully responded to message \"%s\" with %A." description s   |> log.Debug
                        | Failure (exn : exn) -> sprintf "Failed to respond to message \"%s\": %A" description (exn.Message) |> log.Error
            
                    /// Workflow performed when shutting down the agent.
                    let closeAgent closeFunc (replyChannel : AsyncReplyChannel<Choice<unit, exn>>) = async {
                        "Closing agent." |> log.Info
                        let response = closeFunc state
                        response |> replyChannel.Reply
                        logResponse "Close" response }

                    /// Default message-processing loop.
                    let rec loop state = async {
                        let! message = mailbox.Receive()
                        sprintf "Received message: %s." (messageDescription message) |> log.Debug

                        match message with 
                        | Command (_, commandFunc, replyChannel) ->
                            let response = commandFunc state
                            response |> replyChannel.Reply
                            logResponse (messageDescription message) response
                            return! loop state

                        | ValueRequest (_, requestFunc, replyChannel) ->
                            let response = requestFunc state
                            response |> replyChannel.Reply
                            logResponse (messageDescription message) response
                            return! loop state

                        | ObjectRequest (_, requestFunc, replyChannel) ->
                            let response = requestFunc state
                            response |> replyChannel.Reply
                            logResponse (messageDescription message) response
                            return! loop state
            
                        | Close (closeFunc, replyChannel) ->
                            // stop looping once the close message is received
                            do! closeAgent closeFunc replyChannel }
                
                    loop state) }

        /// Posts a command to the message queue which will be executed by calling the provided
        /// function. The command may succeed or fail, and if failure occurs, the enclosed
        /// exception is raised. The provided description is used for logging.    
        let performCommand description commandFunc (CommandRequestAgent agent) = async {
            let! response = agent.PostAndAsyncReply (fun replyChannel -> Command(description, commandFunc, replyChannel))
            match response with
            | Success ()  -> return ()
            | Failure exn -> raise exn }

        /// Posts a request to the message queue which will be executed by calling the provided
        /// function. The request may succeed or fail, and if failure occurs, the enclosed
        /// exception is raised. . If the request is successful, it returns an object type. The
        /// provided description is used for logging.
        let performObjectRequest<'State, 'Result when 'Result :> obj> description (requestFunc : 'State -> Choice<'Result, exn>) (CommandRequestAgent agent) = async {
            let castRequestFunc = requestFunc >> Choice.map (fun s -> s :> obj)
            let! response = agent.PostAndAsyncReply (fun replyChannel -> ObjectRequest(description, castRequestFunc, replyChannel))
            match response with
            | Success s   -> return s :?> 'Result
            | Failure exn -> raise exn ; return Unchecked.defaultof<_> }
    
        /// Posts a request to the message queue which will be executed by calling the provided
        /// function. The request may succeed or fail, and if failure occurs, the enclosed
        /// exception is raised. If the request is successful, it returns a value type. The
        /// provided description is used for logging.    
        let performValueRequest<'State, 'Result when 'Result :> ValueType> description (requestFunc : 'State -> Choice<'Result, exn>) (CommandRequestAgent agent) = async {
            let castRequestFunc = requestFunc >> Choice.map (fun s -> s :> ValueType)
            let! response = agent.PostAndAsyncReply (fun replyChannel ->ValueRequest(description, castRequestFunc, replyChannel))
            match response with
            | Success s   -> return s :?> 'Result
            | Failure exn -> raise exn ; return Unchecked.defaultof<_>  }

        /// Shuts down the message-processing agent after calling the supplied closing function. The
        /// function may succeed or fail, and if failure occurs, the enclosed exception is raised.
        /// Furthermore, an exception will be raised if there are any remaining messages in the queue
        /// at this point.
        let close closeFunc (CommandRequestAgent agent) = async {
            let! response = agent.PostAndAsyncReply (fun replyChannel -> Close(closeFunc, replyChannel))
            match response with
            | Success ()  -> return ()
            | Failure exn -> raise exn }