port module Test.App exposing (..)

import Html exposing (..)
import Html.App
import Time exposing (Time, second)
import Process
import Task exposing (Task)
import CommandHelper
import ParentChildUpdate exposing (..)
import StringUtils exposing ((+-+))
import Utils.Ops exposing (..)


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
    }


pgReconnectDelayInterval : Time
pgReconnectDelayInterval =
    10 * second


delayUpdateMsg : Msg -> Time -> Cmd Msg
delayUpdateMsg msg delay =
    Task.perform (\_ -> Nop) (\_ -> msg) <| Process.sleep delay


type Msg
    = Nop
    | CommandHelperError String
    | CommandHelperLog String
    | InitCommand
    | InitCommandComplete CommandHelper.CommandId
    | LockCommandComplete CommandHelper.CommandId
    | CommandHelperModule CommandHelper.Msg


commandHelperConfig : CommandHelper.Config Msg
commandHelperConfig =
    { pgConnectionInfo = pgConnectionInfo
    , pgReconnectDelayInterval = pgReconnectDelayInterval
    , errorTagger = CommandHelperError
    , logTagger = CommandHelperLog
    , initCommandTagger = InitCommandComplete
    , lockCommandTagger = LockCommandComplete
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
        model ! (List.append cmds [ delayUpdateMsg InitCommand (1 * second) ])


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

            InitCommand ->
                let
                    l =
                        Debug.log "InitCommand" ""

                    ( commandHelperModel, cmd ) =
                        CommandHelper.initCommand commandHelperConfig model.commandHelperModel
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log "initCommand Error:" err
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ Cmd.map CommandHelperModule cmd ]

            InitCommandComplete commandId ->
                let
                    l =
                        Debug.log "InitCommandComplete" ("Command Id:  " +-+ commandId)

                    ( commandHelperModel, cmd ) =
                        CommandHelper.lockEntities commandId
                            [ "7859eea1-d1ee-46be-865d-b04e0a12df4c", "31cd9070-9073-415e-889f-dc0278dc7283" ]
                            commandHelperConfig
                            model.commandHelperModel
                            ??= (\err ->
                                    let
                                        l =
                                            Debug.log "lockEntities Command Error:" err
                                    in
                                        ( model.commandHelperModel, Cmd.none )
                                )
                in
                    { model | commandHelperModel = commandHelperModel } ! [ Cmd.map CommandHelperModule cmd ]

            LockCommandComplete commandId ->
                let
                    l =
                        Debug.log "LockCommandComplete" ("Command Id:  " +-+ commandId)
                in
                    model ! []

            CommandHelperModule msg ->
                updateCommandHelper msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
