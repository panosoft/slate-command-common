module CommandHelper
    exposing
        ( Msg
        , Model
        , Config
        , CommandId
        , PGConnectionInfo
        , init
        , initCommand
        , lockEntities
        , update
        )

import Dict exposing (Dict)
import Process
import Task exposing (Task)
import Time exposing (Time, second)
import Postgres exposing (..)
import ParentChildUpdate exposing (..)
import StringUtils exposing (..)
import Locker exposing (..)


type alias CommandId =
    Int


type alias ConnectionId =
    Int


type alias CommandIdDict =
    Dict CommandId ConnectionId


{-|
    Postgres connection info
-}
type alias PGConnectionInfo =
    { host : String
    , port_ : Int
    , database : String
    , user : String
    , password : String
    , connectTimeout : Int
    }


{-|
parent msg taggers
-}
type alias ErrorTagger msg =
    String -> msg


type alias LogTagger msg =
    String -> msg


type alias InitCommandTagger msg =
    CommandId -> msg


type alias LockCommandTagger msg =
    CommandId -> msg



--


type alias Config msg =
    { pgConnectionInfo : PGConnectionInfo
    , pgReconnectDelayInterval : Time
    , errorTagger : ErrorTagger msg
    , logTagger : LogTagger msg
    , initCommandTagger : InitCommandTagger msg
    , lockCommandTagger : LockCommandTagger msg
    }


lockerConfig : Locker.Config Msg
lockerConfig =
    { errorTagger = LockerError
    , logTagger = LockerLog
    }


delayUpdateMsg : Msg -> Time -> Cmd Msg
delayUpdateMsg msg delay =
    Task.perform (\_ -> Nop) (\_ -> msg) <| Process.sleep delay


delayCmd : Cmd Msg -> Time -> Cmd Msg
delayCmd cmd =
    delayUpdateMsg <| DoCmd cmd


{-|
    Msg
-}
type Msg
    = Nop
    | DoCmd (Cmd Msg)
    | PGConnect CommandId ConnectionId
    | PGConnectError CommandId ( ConnectionId, String )
    | PGConnectionLost CommandId ( ConnectionId, String )
    | PGDisconnectError CommandId ( ConnectionId, String )
    | PGDisconnect CommandId ConnectionId
    | LockCommandSuccess CommandId
    | LockerError String
    | LockerLog String
    | LockerModule Locker.Msg


type alias Model =
    { commandIds : CommandIdDict
    , nextCommandId : CommandId
    , lockerModel : Locker.Model
    }


initModel : ( Model, List (Cmd Msg) )
initModel =
    let
        ( lockerModel, lockerCmd ) =
            Locker.init
    in
        ( { commandIds = Dict.empty
          , nextCommandId = 0
          , lockerModel = lockerModel
          }
        , [ Cmd.map LockerModule lockerCmd ]
        )


{-|
    initialize command helper
-}
init : ( Model, Cmd Msg )
init =
    let
        ( model, cmds ) =
            initModel
    in
        model ! (cmds)


{-|
    update
-}
update : Config msg -> Msg -> Model -> ( ( Model, Cmd Msg ), List msg )
update config msg model =
    let
        logMsg message =
            config.logTagger message

        logErr error =
            config.errorTagger error

        updateLocker =
            ParentChildUpdate.updateChildParent (Locker.update lockerConfig) (update config) .lockerModel LockerModule (\model lockerModel -> { model | lockerModel = lockerModel })
    in
        case msg of
            Nop ->
                ( model ! [], [] )

            DoCmd cmd ->
                ( model ! [ cmd ], [] )

            PGConnect commandId pgConnectionId ->
                let
                    commandIds =
                        Dict.insert commandId pgConnectionId model.commandIds
                in
                    ( { model | commandIds = commandIds } ! []
                    , [ logMsg ("PGConnect:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ pgConnectionId)
                      , config.initCommandTagger commandId
                      ]
                    )

            PGConnectError commandId ( _, pgError ) ->
                ( model ! [], [ logErr ("initCommand Error:" +-+ "Command Id:" +-+ commandId +-+ "Connection Error:" +-+ pgError) ] )

            PGConnectionLost commandId ( pgConnectionId, pgError ) ->
                ( model ! []
                , [ logErr ("PGConnectLost:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ pgConnectionId +-+ "Connection Error:" +-+ pgError) ]
                )

            PGDisconnectError commandId ( pgConnectionId, pgError ) ->
                ( model ! [], [] )

            PGDisconnect commandId pgConnectionId ->
                ( model ! [], [] )

            LockCommandSuccess commandId ->
                ( model ! [], [ config.lockCommandTagger commandId ] )

            LockerError message ->
                ( model ! [], [ config.errorTagger message ] )

            LockerLog message ->
                ( model ! [], [ config.logTagger message ] )

            LockerModule msg ->
                updateLocker msg model


{-|
    API
-}
initCommand : Config msg -> Model -> Result String ( Model, Cmd Msg )
initCommand config model =
    Ok
        ( { model | nextCommandId = model.nextCommandId + 1 }
        , Postgres.connect (PGConnectError model.nextCommandId) (PGConnect model.nextCommandId) (PGConnectionLost model.nextCommandId) config.pgConnectionInfo.connectTimeout config.pgConnectionInfo.host config.pgConnectionInfo.port_ config.pgConnectionInfo.database config.pgConnectionInfo.user config.pgConnectionInfo.password
        )


lockEntities : CommandId -> List String -> Config msg -> Model -> Result String ( Model, Cmd Msg )
lockEntities commandId entities config model =
    let
        maybeConnectionId =
            Dict.get commandId model.commandIds
    in
        case maybeConnectionId of
            Just connectionId ->
                let
                    ( lockerModel, cmd ) =
                        Locker.lock model.lockerModel connectionId entities
                in
                    Ok ( { model | lockerModel = lockerModel }, Cmd.map LockerModule cmd )

            Nothing ->
                Err <| "CommandId:  " ++ (toString commandId) ++ " doesn't exist"
