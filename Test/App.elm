port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Time exposing (Time, second)
import Process
import Task exposing (Task)
import Json.Encode as JE
import StringUtils exposing ((+-+), (+++))
import CommandHelper
import ParentChildUpdate exposing (..)
import Utils.Ops exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import Slate.Common.Db exposing (..)
import Slate.Common.Event exposing (Metadata)
import DebugF exposing (..)


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


entityId1 : String
entityId1 =
    "7859eea1-d1ee-46be-865d-b04e0a12df4c"


entityId2 : String
entityId2 =
    "31cd9070-9073-415e-889f-dc0278dc7283"


type Msg
    = Nop
    | DoCmd (Cmd Msg)
    | CommandHelperError ( ErrorType, String )
    | CommandHelperLog ( LogLevel, String )
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


dbConnectionInfo : DbConnectionInfo
dbConnectionInfo =
    { host = "localPGDbServer"
    , port_ = 5432
    , database = "test_entities"
    , user = "parallels"
    , password = "parallelspw"
    , timeout = 5000
    }


commandHelperConfig : CommandHelper.Config Msg
commandHelperConfig =
    { lockRetries = 3
    , routeToMeTagger = CommandHelperModule
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
        model ! (List.append cmds [ delayUpdateMsg InitCommandStart (1 * second) ])


delayUpdateMsg : Msg -> Time -> Cmd Msg
delayUpdateMsg msg delay =
    Task.perform (\_ -> Nop) (\_ -> msg) <| Process.sleep delay


delayCmd : Cmd Msg -> Time -> Cmd Msg
delayCmd cmd =
    delayUpdateMsg <| DoCmd cmd


main : Program Never
main =
    Html.App.program
        { init = init
        , view = (\_ -> text "")
        , update = update
        , subscriptions = subscriptions
        }


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

            DoCmd cmd ->
                model ! [ cmd ]

            CommandHelperError ( errorType, details ) ->
                let
                    l =
                        case errorType of
                            NonFatalError ->
                                DebugF.log "CommandHelperError" details

                            _ ->
                                Debug.crash <| toString details
                in
                    model ! []

            CommandHelperLog ( logLevel, details ) ->
                let
                    l =
                        DebugF.log "CommandHelperLog" (toString logLevel ++ ":" +-+ details)
                in
                    model ! []

            InitCommandStart ->
                let
                    l =
                        Debug.log "InitCommandStart" "Calling CommandHelper.initCommand"

                    ( commandHelperModel, cmd ) =
                        CommandHelper.initCommand commandHelperConfig dbConnectionInfo model.commandHelperModel
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
                    l =
                        Debug.log "InitCommand Complete" ("Command Id:  " +-+ commandId)

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
                    l =
                        Debug.log "LockEntities Complete" ("Command Id:  " +-+ commandId)

                    events =
                        [ encodeEvent "User Created" entityId1 <| CommandHelper.createMetadata "Create User" "64194fcb-bf87-40c2-bee7-3a86f0110840"
                        , encodeEvent "User Created" entityId2 <| CommandHelper.createMetadata "Create User" "d2a1cf24-dc3a-45d6-8310-1fb6eb184d1b"
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
                    l =
                        Debug.log "WriteEvents Complete" ("Command Id:" +-+ commandId +-+ "Events Inserted:" +-+ eventRows)

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
                    ( model, delayCmd (exitApp 0) (1 * second) )

            CommitError ( commandId, error ) ->
                let
                    l =
                        Debug.log "Commit Complete with Error" ("Command Id:" +-+ commandId +-+ "Error:" +-+ error)
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            Rollback commandId ->
                let
                    l =
                        Debug.log "Rollback Complete" ("Command Id:  " +-+ commandId)
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            RollbackError ( commandId, error ) ->
                let
                    l =
                        Debug.log "Rollback Complete with Error" ("Command Id:  " +-+ commandId +-+ "Error:" +-+ error)
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            ConnectionLost ( commandId, error ) ->
                let
                    l =
                        Debug.log "ConnectionLost" ("Command Id:  " +-+ commandId +-+ "Error:" +-+ error)
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            CommandHelperModule msg ->
                updateCommandHelper msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


encodeMetadata : Metadata -> JE.Value
encodeMetadata metadata =
    JE.object [ ( "command", JE.string metadata.command ), ( "initiatorId", JE.string metadata.initiatorId ) ]


encodeEvent : String -> String -> Metadata -> String
encodeEvent name entityId metadata =
    JE.encode 0 <|
        JE.object
            [ ( "name", JE.string name )
            , ( "data", JE.object [ ( "entityId", JE.string entityId ) ] )
            , ( "metadata", encodeMetadata metadata )
            ]
