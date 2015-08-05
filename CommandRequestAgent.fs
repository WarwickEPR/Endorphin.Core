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
        /// Choice<'Result, 'Error> where the error message is a string. Commands do not return a
        /// vaule in the success case, so the result type is unit. Requests can either return a
        /// value type or an object type. Command and request messages also include a description
        /// which is used for logging. Finally, the close message notifies the agent that it
        /// should stop processing messages.
        type internal CommandRequestMessage<'State> = 
            | Command       of description : string * commandFunc : ('State -> Choice<unit,      string>) * replyChannel : AsyncReplyChannel<Choice<unit,      string>>
            | ValueRequest  of description : string * requestFunc : ('State -> Choice<ValueType, string>) * replyChannel : AsyncReplyChannel<Choice<ValueType, string>>
            | ObjectRequest of description : string * requestFunc : ('State -> Choice<obj,       string>) * replyChannel : AsyncReplyChannel<Choice<obj,       string>>
            | Close         of                        closeFunc   : ('State -> Choice<unit,      string>) * replyChannel : AsyncReplyChannel<Choice<unit,      string>>
    
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
        /// processingof any given message may fail. If the agent is set to persist failure,
        /// then it will enter afailed state and return failure to all future messages.
        let create<'State> (nameFunc : 'State -> string) persistFailure (init : unit -> AsyncChoice<'State, string>) = asyncChoice { 
            let! state = init () // perform initialisation
            return CommandRequestAgent // if it succeeds, start the mailbox processing loop
            <| Agent.Start(fun (mailbox : Agent<CommandRequestMessage<'State>>) ->
                let log = LogManager.GetLogger (nameFunc state) // create a logger
                
                let logResponse description = function
                    | Success s -> sprintf "Successfully responded to message \"%s\" with %A." description s |> log.Debug
                    | Failure f -> sprintf "Failed to respond to message \"%s\" due to error: %A." description f |> log.Error
            
                /// Workflow performed when shutting down the agent.
                let closeAgent closeFunc (replyChannel : AsyncReplyChannel<Choice<unit, string>>) = async {
                    "Closing agent." |> log.Info

                    // perform the closing function and respond accordingly
                    let response =
                        match closeFunc state with
                        | Failure f ->
                            fail <| sprintf "Failed to close agent due to error: %s" f
                        | Success () when mailbox.CurrentQueueLength <> 0 ->
                            fail <| sprintf "Closing agent with %d messages remaining in mailbox queue." mailbox.CurrentQueueLength 
                        | Success () -> succeed ()

                    response |> replyChannel.Reply
                    logResponse "Close" response }
            
                /// Message-processing loop which replies to future messages with failure after an
                /// error has occured. The provided error description is used in the description of
                /// the failure when performing commands and requests and for logging.
                let rec failed error = async {
                    let! message = mailbox.Receive()
                    let errorMessage = sprintf "Received message \"%s\" after agent failed due to error: %s" (messageDescription message) error
                
                    match message with
                    | Command (_, _, replyChannel) ->
                        let response = fail errorMessage
                        response |> replyChannel.Reply
                        logResponse (messageDescription message) response
                        return! failed error
                
                    | ValueRequest (_, _, replyChannel) ->
                        let response = fail errorMessage
                        response |> replyChannel.Reply
                        logResponse (messageDescription message) response
                        return! failed error

                    | ObjectRequest (_, _, replyChannel) ->
                        let response = fail errorMessage
                        response |> replyChannel.Reply
                        logResponse (messageDescription message) response
                        return! failed error
                
                    | Close (closeFunc, replyChannel) -> 
                        // shutting down the agent is the only valid message in this state
                        do! closeAgent closeFunc replyChannel }

                /// Default message-processing loop.
                let rec loop state = async {
                    let! message = mailbox.Receive()
                    sprintf "Received message: %s." (messageDescription message) |> log.Debug
                
                    // continues the workflow after a message is processed, either remaining in the
                    // default loop or entering the failed loop depending on the result
                    let continueAfter response = async { 
                        match response with
                        | Failure f when persistFailure -> return! failed f
                        | _                             -> return! loop state }

                    match message with 
                    | Command (_, commandFunc, replyChannel) ->
                        let response = commandFunc state
                        response |> replyChannel.Reply
                        logResponse (messageDescription message) response
                        return! continueAfter response

                    | ValueRequest (_, requestFunc, replyChannel) ->
                        let response = requestFunc state
                        response |> replyChannel.Reply
                        logResponse (messageDescription message) response
                        return! continueAfter response

                    | ObjectRequest (_, requestFunc, replyChannel) ->
                        let response = requestFunc state
                        response |> replyChannel.Reply
                        logResponse (messageDescription message) response
                        return! continueAfter response
            
                    | Close (closeFunc, replyChannel) ->
                        // stop looping once the close message is received
                        do! closeAgent closeFunc replyChannel }
                
                loop state) }

        /// Posts a command to the message queue which will be executed by calling the provided
        /// function. The command may succeed or fail, which will be reflected in the asynchronous
        /// reply. All future messages will automatically result in failure if the command fails
        /// and the agent is set to persist failure. The supplied description is used for logging.
        let performCommand description commandFunc (CommandRequestAgent agent) =
            agent.PostAndAsyncReply (fun replyChannel -> Command(description, commandFunc, replyChannel))

        /// Posts a request to the message queue which will be executed by calling the provided
        /// function. The request may succeed or fail, which will be reflected in the asynchronous
        /// reply. If the request is successful, it returns an object type. All future messages will
        /// automatically result in failure if the command fails and the agent is set to persist
        /// failure. The supplied description is used for logging.
        let performObjectRequest<'State, 'Result when 'Result :> obj> description (requestFunc : 'State -> Choice<'Result, string>) (CommandRequestAgent agent) = async {
            let castRequestFunc = requestFunc >> Choice.map (fun s -> s :> obj)
            let! response = agent.PostAndAsyncReply (fun replyChannel -> ObjectRequest(description, castRequestFunc, replyChannel))
            return response |> Choice.map (fun s -> s :?> 'Result) }
    
        /// Posts a request to the message queue which will be executed by calling the provided
        /// function. The request may succeed or fail, which will be reflected in the asynchronous
        /// reply. If the request is successful, it returns a value type. All future messages will
        /// automatically result in failure if the command fails and the agent is set to persist
        /// failure. The supplied description is used for logging.    
        let performValueRequest<'State, 'Result when 'Result :> ValueType> description (requestFunc : 'State -> Choice<'Result, string>) (CommandRequestAgent agent) = async {
            let castRequestFunc = requestFunc >> Choice.map (fun s -> s :> ValueType)
            let! response = agent.PostAndAsyncReply (fun replyChannel ->ValueRequest(description, castRequestFunc, replyChannel))
            return response |> Choice.map (fun s -> s :?> 'Result) }

        /// Shuts down the message-processing agent after calling the supplied closing function. The
        /// function may succeed or fail, which will determine the asynchronously-returned result.
        /// Furthermore, the result will be failure if there are any remaining messages in the queue
        /// after this point.
        let close closeFunc (CommandRequestAgent agent) =
            agent.PostAndAsyncReply (fun replyChannel -> Close(closeFunc, replyChannel))