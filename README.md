# Common Command Processor Modules for Slate

> Provides helper functions for writing Slate Entity Command Processors.

## Install

### Elm

Since the Elm Package Manager doesn't allow for Native code and most everything we write at Panoramic Software has some native code in it,
you have to install this library directly from GitHub, e.g. via [elm-github-install](https://github.com/gdotdesign/elm-github-install) or some equivalent mechanism. It's just not worth the hassle of putting libraries into the Elm package manager until it allows native code.

## API

* [initCommand](#initCommand)
* [lockEntities](#lockEntities)
* [writeEvents](#writeEvents)
* [commit](#commit)
* [rollback](#rollback)
* [createMetadata](#createMetadata)


The API is used to facilitate Entity Command Processor command validation and writing events to the events table for events created by a command.


An Entity Command Processor will use the API in the following way for each command it needs to process:

1. Call `initCommand` to begin a transaction and get a `commandId` for subsequent API calls.
2. Optionally call `lockEntities` to perform validation for Entities referenced by the command being processed.
3. Call `writeEvents` to write events to the events table for events created by the command being processed. The createMetadata API function can be used to help create part of each event written.
4. Either call `commit` to commit the events written to the events table and release any Entity locks obtained, or `rollback` to abort writing events to the events table and release any Entity locks obtained.


### initCommand

> Prepares a command used to insert events into an events table.  Causes a `commandId` to be returned to the caller.

```elm
initCommand : Config msg -> Model -> Result String ( Model, Cmd msg )
initCommand config model
```
### lockEntities

> Locks Entities using `Postgresql` advisory locks.  Used to prevent events being written to the events table for Entities during command validation.

```elm
lockEntities : Config msg -> Model -> CommandId -> List String -> Result String ( Model, Cmd msg )
lockEntities config model commandId entityIds
```
### writeEvents

> Write events to the events table of the target database.

```elm
writeEvents : Config msg -> Model -> CommandId -> List String -> Result String ( Model, Cmd msg )
writeEvents config model commandId events
```
### commit

> Commits the transaction started by initCommand causing written events to be committed and any Entity locks to be released.

```elm
commit : Config msg -> Model -> CommandId -> Result String ( Model, Cmd msg )
commit config model commandId
```

### rollback

> Aborts the transaction started by initCommand causing written events to be rolled back and any Entity locks to be released.

```elm
rollback : Config msg -> Model -> CommandId -> Result String ( Model, Cmd msg )
rollback config model commandId
```

### createMetadata

> Creates the record for the `metadata` field in an event.

```elm
createMetadata : String -> String -> Metadata
createMetadata initiatorId command
```

## Example

Below is example code using the Common Command Processor API:


``` elm
import CommandHelper
import Time exposing (Time, second)
import Process
import Task exposing (Task)
import CommandHelper
import ParentChildUpdate exposing (..)
import StringUtils exposing ((+-+), (+++))
import Utils.Ops exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import Json.Encode as JE

type alias Metadata =
    { initiatorId : String
    , command : String
    }

type Msg
    = Nop
    | CommandHelperError ( ErrorType, ( CommandHelper.CommandId, String ) )
    | CommandHelperLog ( LogLevel, ( CommandHelper.CommandId, String ) )
    | InitCommandStart
    | InitCommand CommandHelper.CommandId
    | InitCommandError ( CommandHelper.CommandId, String )
    | LockEntities CommandHelper.CommandId
    | LockEntitiesError ( CommandHelper.CommandId, String )
    | WriteEvents ( CommandHelper.CommandId, Int )
    | WriteEventsError ( CommandHelper.CommandId, String )
    | Commit CommandHelper.CommandId
    | CommitError ( CommandHelper.CommandId, String )
    | Rollback CommandHelper.CommandId
    | RollbackError ( CommandHelper.CommandId, String )
    | ConnectionLost ( CommandHelper.CommandId, String )
    | CommandHelperModule CommandHelper.Msg

type alias DbConnectionInfo =
    { host : String
    , port_ : Int
    , database : String
    , user : String
    , password : String
    , timeout : Int
    }
dbConnectionInfo : DbConnectionInfo
dbConnectionInfo =
    { host = "server"
    , port_ = 5432
    , database = "testDB"
    , user = "user"
    , password = "password"
    , timeout = 5000
    }

commandHelperConfig : CommandHelper.Config Msg
commandHelperConfig =
    { dbConnectionInfo = dbConnectionInfo
    , commandHelperTagger = CommandHelperModule
    , errorTagger = CommandHelperError
    , logTagger = CommandHelperLog
    , initCommandTagger = InitCommand
    , initCommandErrorTagger = InitCommandError
    , lockEntitiesTagger = LockEntities
    , lockEntitiesErrorTagger = LockEntitiesError
    , writeEventsTagger = WriteEvents
    , writeEventsErrorTagger = WriteEventsError
    , commitTagger = Commit
    , commitErrorTagger = CommitError
    , rollbackTagger = Rollback
    , rollbackErrorTagger = RollbackError
    , connectionLostTagger = ConnectionLost
    }

type alias Model =
    { commandHelperModel : CommandHelper.Model
    }


initModel : ( Model, List (Cmd Msg) )
initModel =
    let
        ( commandHelperModel, commandHelperCmd ) =
            CommandHelper.init CommandHelperModule
    in
        ( { commandHelperModel = commandHelperModel
          }
        , [ commandHelperCmd ]
        )


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmds ) =
            initModel
    in
        model ! (List.append cmds [ Task.perform (\_ -> Nop) (\_ -> InitCommandStart) <| Process.sleep (1 * second) InitCommandStart]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateCommandHelper : CommandHelper.Msg -> Model -> ( Model, Cmd Msg )
        updateCommandHelper =
            ParentChildUpdate.updateChildApp (CommandHelper.update commandHelperConfig) update .commandHelperModel CommandHelperModule (\model commandHelperModel -> { model | commandHelperModel = commandHelperModel })
    in
        case msg of
            Nop ->
                model ! []

            CommandHelperError ( errorType, details ) ->
                let
                    l =
                        case errorType of
                            NonFatalError ->
                                Debug.log "CommandHelperError" details

                            _ ->
                                Debug.crash <| toString details
                in
                    model ! []

            CommandHelperLog ( logLevel, details ) ->
                let
                    l =
                        Debug.log "CommandHelperLog" (toString logLevel ++ ":" +-+ details)
                in
                    model ! []

            {-
                The following Msg created by init function is used to start a test that uses the CommandHelper API
            -}
            InitCommandStart ->
                let
                    ( commandHelperModel, cmd ) =
                        CommandHelper.initCommand commandHelperConfig model.commandHelperModel
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log ("CommandHelper.initCommand call returned Error:" +-+ err)
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ cmd ]

            InitCommand commandId ->
                let
                    ( commandHelperModel, cmd ) =
                        CommandHelper.lockEntities commandHelperConfig model.commandHelperModel commandId [ entityId1, entityId2 ]
                            ??= (\err ->
                                    Debug.crash ("CommandHelper.lockEntities call returned Error:" +-+ err +-+ "CommandId:" +-+ commandId)
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ cmd ]

            InitCommandError ( commandId, error ) ->
                let
                    l =
                        Debug.log "InitCommand Complete with Error" ("Command Id:  " +-+ commandId +-+ "Error:" +-+ error)
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            LockEntities commandId ->
                let
                    events =
                        [ encodeEvent "User Created" entityId1 "Create User" "64194fcb-bf87-40c2-bee7-3a86f0110840"
                        , encodeEvent "User Created" entityId2 "Create User" "d2a1cf24-dc3a-45d6-8310-1fb6eb184d1b"
                        ]

                    ( commandHelperModel, cmd ) =
                        CommandHelper.writeEvents commandHelperConfig model.commandHelperModel commandId events
                            ??= (\err ->
                                    Debug.crash ("CommandHelper.writeEvents call returned Error:" +-+ err +-+ "CommandId:" +-+ commandId)
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ cmd ]

            LockEntitiesError ( commandId, error ) ->
                let
                    l =
                        Debug.log "LockEntities Complete with Error" ("Command Id:  " +-+ commandId +-+ "Error:" +-+ error)

                    ( commandHelperModel, cmd ) =
                        CommandHelper.rollback commandHelperConfig model.commandHelperModel commandId
                            ??= (\err ->
                                    Debug.crash ("CommandHelper.rollback call returned Error:" +-+ err +-+ "CommandId:" +-+ commandId)
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ cmd ]

            WriteEvents ( commandId, eventRows ) ->
                let
                    ( commandHelperModel, cmd ) =
                        CommandHelper.commit commandHelperConfig model.commandHelperModel commandId
                            ??= (\err ->
                                    Debug.crash ("CommandHelper.commit call returned Error:" +-+ err +-+ "CommandId:" +-+ commandId)
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ cmd ]

            WriteEventsError ( commandId, error ) ->
                let
                    l =
                        Debug.log "WriteEvents Complete with Error" ("Command Id:" +-+ commandId +-+ "Error:" +-+ error)

                    ( commandHelperModel, cmd ) =
                        CommandHelper.rollback commandHelperConfig model.commandHelperModel commandId
                            ??= (\err ->
                                    Debug.crash ("CommandHelper.rollback call returned Error:" +-+ err +-+ "CommandId:" +-+ commandId)
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ cmd ]

            Commit commandId ->
                let
                    l =
                        Debug.log "Commit Complete" ("Command Id:  " +-+ commandId)
                in
                    model ! []

            CommitError ( commandId, error ) ->
                let
                    l =
                        Debug.log "Commit Complete with Error" ("Command Id:" +-+ commandId +-+ "Error:" +-+ error)
                in
                    model ! []

            Rollback commandId ->
                model ! []

            RollbackError ( commandId, error ) ->
                let
                    l =
                        Debug.log "Rollback Complete with Error" ("Command Id:  " +-+ commandId +-+ "Error:" +-+ error)
                in
                    model ! []

            ConnectionLost ( commandId, error ) ->
                let
                    l =
                        Debug.log "ConnectionLost" ("Command Id:  " +-+ commandId +-+ "Error:" +-+ error)
                in
                    model ! []

            CommandHelperModule msg ->
                updateCommandHelper msg model


encodeEvent : String -> String -> String -> String -> String
encodeEvent name entityId command initiatorId =
    JE.encode 0 <|
        JE.object
            [ ( "name", JE.string name )
            , ( "data", JE.object [ ( "entityId", JE.string entityId ) ] )
            , ( "metadata", JE.object [ ( "command", JE.string command ), ( "initiatorId", JE.string initiatorId ) ] )
            ]

```
