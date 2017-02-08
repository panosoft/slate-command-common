port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Time exposing (Time, second)
import Process
import Task exposing (Task)
import ParentChildUpdate exposing (..)
import Slate.Common.Db exposing (..)
import Slate.Command.Common.Command exposing (..)
import Slate.Command.Processor as CommandProcessor
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import StringUtils exposing ((+-+), (+++))
import DebugF exposing (..)


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


dbConnectionInfo : DbConnectionInfo
dbConnectionInfo =
    { host = "postgresDBServer"
    , port_ = 5432
    , database = "test_entities"
    , user = "charles"
    , password = "testpassword"
    , timeout = 5000
    }


commandProcessorConfig : CommandProcessor.Config Msg
commandProcessorConfig =
    { routeToMeTagger = CommandProcessorModule
    , errorTagger = CommandProcessorError
    , logTagger = CommandProcessorLog
    , commandErrorTagger = CommandError
    , commandSuccessTagger = CommandSuccess
    }


type alias Model =
    { commandProcessorModel : CommandProcessor.Model Msg
    }


type Msg
    = Nop
    | DoCmd (Cmd Msg)
    | StartApp
    | CommandProcessorError ( ErrorType, ( CommandId, String ) )
    | CommandProcessorLog ( LogLevel, ( CommandId, String ) )
    | CommandProcessorModule CommandProcessor.Msg
    | CommandError ( CommandId, String )
    | CommandSuccess CommandId


initModel : ( Model, List (Cmd Msg) )
initModel =
    let
        ( commandProcessorModel, commandProcessorCmd ) =
            CommandProcessor.init commandProcessorConfig
    in
        ( { commandProcessorModel = commandProcessorModel
          }
        , [ commandProcessorCmd ]
        )


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmds ) =
            initModel
    in
        model ! (List.append cmds [ delayUpdateMsg StartApp (1 * second) ])


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
        updateCommandProcessor =
            ParentChildUpdate.updateChildApp (CommandProcessor.update commandProcessorConfig) update .commandProcessorModel CommandProcessorModule (\model commandProcessorModel -> { model | commandProcessorModel = commandProcessorModel })
    in
        case msg of
            Nop ->
                model ! []

            DoCmd cmd ->
                model ! [ cmd ]

            StartApp ->
                let
                    lockEntityIds =
                        [ "123"
                        ]

                    events =
                        [ "{\"data\": {\"entityId\": \"123\"}, \"name\": \"Person created\", \"metadata\": {\"command\": \"Create person\", \"initiatorId\": \"999888777\"}}"
                        ]

                    ( commandProcessorModel, cmd, commandId ) =
                        CommandProcessor.process commandProcessorConfig dbConnectionInfo model.commandProcessorModel Nothing lockEntityIds events
                in
                    { model | commandProcessorModel = commandProcessorModel } ! [ cmd ]

            CommandProcessorError ( errorType, details ) ->
                let
                    l =
                        case errorType of
                            NonFatalError ->
                                DebugF.log "CommandProcessorError" details

                            _ ->
                                Debug.crash <| toString details
                in
                    model ! []

            CommandProcessorLog ( logLevel, details ) ->
                let
                    l =
                        DebugF.log "CommandProcessorLog" (toString logLevel ++ ":" +-+ details)
                in
                    model ! []

            CommandError ( commandId, error ) ->
                let
                    l =
                        Debug.log "CommandError" ( commandId, error )
                in
                    model ! []

            CommandSuccess commandId ->
                let
                    l =
                        Debug.log "CommandSuccess" commandId
                in
                    model ! []

            CommandProcessorModule msg ->
                updateCommandProcessor msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
