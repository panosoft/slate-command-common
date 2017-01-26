port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Time exposing (Time, second)
import Process
import Task exposing (Task)
import CommandHelper
import ParentChildUpdate exposing (..)
import StringUtils exposing ((+-+), (+++))
import Utils.Ops exposing (..)
import Json.Encode as JE


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


pgConnectionInfo : CommandHelper.PGConnectionInfo
pgConnectionInfo =
    { host = "localPGDbServer"
    , port_ = 5432
    , database = "parallelsTest"
    , user = "parallels"
    , password = "parallelspw"
    , connectTimeout = 5000
    , retries = 3
    , reconnectDelayInterval = 7 * second
    }


entityId1 : String
entityId1 =
    "7859eea1-d1ee-46be-865d-b04e0a12df4c"


entityId2 : String
entityId2 =
    "31cd9070-9073-415e-889f-dc0278dc7283"


delayUpdateMsg : Msg -> Time -> Cmd Msg
delayUpdateMsg msg delay =
    Task.perform (\_ -> Nop) (\_ -> msg) <| Process.sleep delay


delayCmd : Cmd Msg -> Time -> Cmd Msg
delayCmd cmd =
    delayUpdateMsg <| DoCmd cmd


type Msg
    = Nop
    | DoCmd (Cmd Msg)
    | CommandHelperError String
    | CommandHelperLog String
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
    | CommandHelperModule CommandHelper.Msg


commandHelperConfig : CommandHelper.Config Msg
commandHelperConfig =
    { pgConnectionInfo = pgConnectionInfo
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
    }


type alias Model =
    { commandHelperModel : CommandHelper.Model
    }


initModel : ( Model, List (Cmd Msg) )
initModel =
    let
        ( commandHelperModel, commandHelperCmd ) =
            CommandHelper.init
    in
        ( { commandHelperModel = commandHelperModel
          }
        , [ Cmd.map CommandHelperModule commandHelperCmd ]
        )


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmds ) =
            initModel
    in
        model ! (List.append cmds [ delayUpdateMsg InitCommandStart (1 * second) ])


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

            CommandHelperError error ->
                let
                    l =
                        Debug.log "CommandHelperError" error
                in
                    model ! []

            CommandHelperLog message ->
                let
                    l =
                        Debug.log "CommandHelperLog" message
                in
                    model ! []

            InitCommandStart ->
                let
                    l =
                        Debug.log "InitCommandStart" ""

                    ( commandHelperModel, cmd ) =
                        CommandHelper.initCommand model.commandHelperModel commandHelperConfig.pgConnectionInfo
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log "initCommand Error:" err
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ Cmd.map CommandHelperModule cmd ]

            InitCommand commandId ->
                let
                    l =
                        Debug.log "InitCommand" ("Command Id:  " +-+ commandId)

                    ( commandHelperModel, cmd ) =
                        CommandHelper.lockEntities model.commandHelperModel commandId [ entityId1, entityId2 ]
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log "lockEntities Command Error:" err
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ Cmd.map CommandHelperModule cmd ]

            InitCommandError ( commandId, error ) ->
                let
                    l =
                        Debug.log "InitCommandError" error
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            LockEntities commandId ->
                let
                    l =
                        Debug.log "LockEntities" ("Command Id:  " +-+ commandId)

                    events =
                        [ encodeEvent "User Created" entityId1 "Create User" "64194fcb-bf87-40c2-bee7-3a86f0110840"
                        , encodeEvent "User Created" entityId2 "Create User" "d2a1cf24-dc3a-45d6-8310-1fb6eb184d1b"
                        ]

                    ( commandHelperModel, cmd ) =
                        CommandHelper.writeEvents model.commandHelperModel commandId events
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log "writeEvents Command Error:" err
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ Cmd.map CommandHelperModule cmd ]

            LockEntitiesError ( commandId, error ) ->
                let
                    l =
                        Debug.log "LockEntitiesError" error

                    ( commandHelperModel, cmd ) =
                        CommandHelper.rollback model.commandHelperModel commandId
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log "rollback Command Error:" err
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ Cmd.map CommandHelperModule cmd ]

            WriteEvents ( commandId, eventRows ) ->
                let
                    l =
                        Debug.log "WriteEvents" ("Command Id:" +-+ commandId +-+ "Events Inserted:" +-+ eventRows)

                    ( commandHelperModel, cmd ) =
                        CommandHelper.commit model.commandHelperModel commandId
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log "commit Command Error:" err
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ Cmd.map CommandHelperModule cmd ]

            WriteEventsError ( commandId, error ) ->
                let
                    l =
                        Debug.log "WriteEventsError" ("Command Id:" +-+ commandId +-+ "Error:" +-+ error)

                    ( commandHelperModel, cmd ) =
                        CommandHelper.rollback model.commandHelperModel commandId
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log "rollback Command Error:" err
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ Cmd.map CommandHelperModule cmd ]

            Commit commandId ->
                let
                    l =
                        Debug.log "Commit" ("Command Id:  " +-+ commandId)
                in
                    ( model, delayCmd (exitApp 0) (1 * second) )

            CommitError ( commandId, error ) ->
                let
                    l =
                        Debug.log "CommitError" error
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            Rollback commandId ->
                let
                    l =
                        Debug.log "Rollback" ("Command Id:  " +-+ commandId)
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            RollbackError ( commandId, error ) ->
                let
                    l =
                        Debug.log "RollbackError" error
                in
                    ( model, delayCmd (exitApp 1) (1 * second) )

            CommandHelperModule msg ->
                updateCommandHelper msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


encodeEvent : String -> String -> String -> String -> String
encodeEvent name entityId command initiatorId =
    JE.encode 0 <|
        JE.object
            [ ( "name", JE.string name )
            , ( "data", JE.object [ ( "entityId", JE.string entityId ) ] )
            , ( "metadata", JE.object [ ( "command", JE.string command ), ( "initiatorId", JE.string initiatorId ) ] )
            ]
