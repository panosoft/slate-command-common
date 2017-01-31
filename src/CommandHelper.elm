module CommandHelper
    exposing
        ( Msg
        , Model
        , Config
        , CommandId
        , PGConnectionConfig
        , init
        , initCommand
        , lockEntities
        , writeEvents
        , commit
        , rollback
        , update
        )

import Dict exposing (Dict)
import Json.Decode as JD exposing (..)
import Time exposing (Time, second)
import Process
import Task exposing (Task)
import String exposing (join)
import Postgres exposing (..)
import ParentChildUpdate exposing (..)
import StringUtils exposing ((+-+), (+++))
import Utils.Json exposing ((<||))
import Utils.Ops exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import Locker exposing (..)


{-|
    TODO Remove this and import it from slate-common.
-}
type alias Metadata =
    { initiatorId : String
    , command : String
    }


type alias CommandId =
    Int


type alias CommandIdDict =
    Dict CommandId ConnectionId


type alias InsertEventsResponse =
    { insert_events : Int
    }


insertEventsResponseDecoder : JD.Decoder InsertEventsResponse
insertEventsResponseDecoder =
    JD.succeed InsertEventsResponse
        <|| ("insert_events" := int)


delayUpdateMsg : Msg -> Time -> Cmd Msg
delayUpdateMsg msg delay =
    Task.perform (\_ -> Nop) (\_ -> msg) <| Process.sleep delay


delayCmd : Cmd Msg -> Time -> Cmd Msg
delayCmd cmd =
    delayUpdateMsg <| DoCmd cmd


{-|
    Postgres connection config
-}
type alias PGConnectionConfig =
    { host : String
    , port_ : Int
    , database : String
    , user : String
    , password : String
    , connectTimeout : Int
    , retries : Int
    , reconnectDelayInterval : Time
    }


{-|
    parent msg taggers
-}
type alias ErrorTagger msg =
    ( ErrorType, ( CommandId, String ) ) -> msg


type alias LogTagger msg =
    ( LogLevel, ( CommandId, String ) ) -> msg


type alias InitCommandTagger msg =
    CommandId -> msg


type alias InitCommandErrorTagger msg =
    ( CommandId, String ) -> msg


type alias LockEntitiesTagger msg =
    CommandId -> msg


type alias LockEntitiesErrorTagger msg =
    ( CommandId, String ) -> msg


type alias WriteEventsTagger msg =
    ( CommandId, Int ) -> msg


type alias WriteEventsErrorTagger msg =
    ( CommandId, String ) -> msg


type alias CommitTagger msg =
    CommandId -> msg


type alias CommitErrorTagger msg =
    ( CommandId, String ) -> msg


type alias RollbackTagger msg =
    CommandId -> msg


type alias RollbackErrorTagger msg =
    ( CommandId, String ) -> msg


type alias ConnectionLostTagger msg =
    ( CommandId, String ) -> msg


type alias Config msg =
    { pgConnectionConfig : PGConnectionConfig
    , lockRetries : Int
    , errorTagger : ErrorTagger msg
    , logTagger : LogTagger msg
    , initCommandTagger : InitCommandTagger msg
    , initCommandErrorTagger : InitCommandErrorTagger msg
    , lockEntitiesTagger : LockEntitiesTagger msg
    , lockEntitiesErrorTagger : LockEntitiesErrorTagger msg
    , writeEventsTagger : WriteEventsTagger msg
    , writeEventsErrorTagger : WriteEventsErrorTagger msg
    , commitTagger : CommitTagger msg
    , commitErrorTagger : CommitErrorTagger msg
    , rollbackTagger : RollbackTagger msg
    , rollbackErrorTagger : RollbackErrorTagger msg
    , connectionLostTagger : ConnectionLostTagger msg
    }


lockerConfig : Config msg -> Locker.Config Msg
lockerConfig config =
    { retries = config.lockRetries
    , errorTagger = LockerError
    , logTagger = LockerLog
    , lockEntitiesTagger = LockEntities
    , lockEntitiesErrorTagger = LockEntitiesError
    }


{-|
    Msg
-}
type Msg
    = Nop
    | DoCmd (Cmd Msg)
    | PGConnect CommandId ConnectionId
    | PGConnectError CommandId Int ( ConnectionId, String )
    | PGConnectionLost CommandId ( ConnectionId, String )
    | PGDisconnectError CommandId ( ConnectionId, String )
    | PGDisconnect CommandId ConnectionId
    | LockEntities CommandId
    | LockEntitiesError ( CommandId, String )
    | Begin CommandId String ( ConnectionId, List String )
    | BeginError CommandId String ( ConnectionId, String )
    | Commit CommandId ( ConnectionId, List String )
    | CommitError CommandId ( ConnectionId, String )
    | Rollback CommandId ( ConnectionId, List String )
    | RollbackError CommandId ( ConnectionId, String )
    | WriteEvents CommandId String ( ConnectionId, List String )
    | WriteEventsError CommandId String ( ConnectionId, String )
    | LockerError ( ErrorType, ( CommandId, String ) )
    | LockerLog ( LogLevel, ( CommandId, String ) )
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
        logMsg commandId message =
            config.logTagger ( LogLevelInfo, ( commandId, message ) )

        nonFatal commandId error =
            config.errorTagger ( NonFatalError, ( commandId, error ) )

        fatal commandId error =
            config.errorTagger ( FatalError, ( commandId, error ) )

        updateLocker =
            ParentChildUpdate.updateChildParent (Locker.update <| lockerConfig config) (update config) .lockerModel LockerModule (\model lockerModel -> { model | lockerModel = lockerModel })
    in
        case msg of
            Nop ->
                ( model ! [], [] )

            DoCmd cmd ->
                ( model ! [ cmd ], [] )

            PGConnect commandId connectionId ->
                let
                    commandIds =
                        Dict.insert commandId connectionId model.commandIds
                in
                    ( { model | commandIds = commandIds } ! []
                    , [ logMsg commandId ("PGConnect:" +-+ "Connection Id:" +-+ connectionId)
                      , config.initCommandTagger commandId
                      ]
                    )

            PGConnectError commandId retryCount ( _, error ) ->
                let
                    ( cmd, appMsgs ) =
                        (retryCount <= config.pgConnectionConfig.retries)
                            ? ( ( delayCmd (connectCmd config commandId (retryCount + 1)) config.pgConnectionConfig.reconnectDelayInterval
                                , [ nonFatal commandId ("initCommand Error:" +-+ "Connection Error:" +-+ error +-+ "Connection Retry:" +-+ retryCount) ]
                                )
                              , ( Cmd.none, [ config.initCommandErrorTagger ( commandId, error ) ] )
                              )
                in
                    ( model ! [ cmd ], appMsgs )

            PGConnectionLost commandId ( connectionId, error ) ->
                let
                    commandIds =
                        Dict.remove commandId model.commandIds

                    errMsg =
                        "PGConnectLost:" +-+ "Connection Id:" +-+ connectionId +-+ "Connection Error:" +-+ error
                in
                    ( { model | commandIds = commandIds } ! []
                    , [ nonFatal commandId errMsg, config.connectionLostTagger ( commandId, errMsg ) ]
                    )

            PGDisconnectError commandId ( connectionId, error ) ->
                let
                    commandIds =
                        Dict.remove commandId model.commandIds

                    parentMsgs =
                        [ nonFatal commandId ("PGDisconnectError:" +-+ "Connection Id:" +-+ connectionId +-+ "Connection Error:" +-+ error) ]
                in
                    ( { model | commandIds = commandIds } ! [], parentMsgs )

            PGDisconnect commandId connectionId ->
                let
                    commandIds =
                        Dict.remove commandId model.commandIds
                in
                    ( { model | commandIds = commandIds } ! [], [] )

            LockEntities commandId ->
                ( model ! [], [ config.lockEntitiesTagger commandId ] )

            LockEntitiesError ( commandId, error ) ->
                let
                    errMsg =
                        nonFatal commandId ("LockEntitiesError:" +-+ "Error:" +-+ error)
                in
                    ( model ! [], [ errMsg, config.lockEntitiesErrorTagger ( commandId, error ) ] )

            Begin commandId statement ( connectionId, results ) ->
                let
                    cmd =
                        Postgres.query (WriteEventsError commandId statement) (WriteEvents commandId statement) connectionId statement 2
                in
                    ( model ! [ cmd ], [] )

            BeginError commandId statement ( connectionId, error ) ->
                let
                    errMsg =
                        nonFatal commandId ("BeginError:" +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
                in
                    ( model ! [], [ errMsg, config.writeEventsErrorTagger ( commandId, error ) ] )

            Commit commandId ( connectionId, results ) ->
                let
                    cmd =
                        Postgres.disconnect (PGDisconnectError commandId) (PGDisconnect commandId) connectionId False
                in
                    ( model ! [ cmd ], [ config.commitTagger commandId ] )

            CommitError commandId ( connectionId, error ) ->
                let
                    cmd =
                        Postgres.disconnect (PGDisconnectError commandId) (PGDisconnect commandId) connectionId True

                    errMsg =
                        nonFatal commandId ("CommitError:" +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
                in
                    ( model ! [], [ errMsg, config.commitErrorTagger ( commandId, error ) ] )

            Rollback commandId ( connectionId, results ) ->
                let
                    cmd =
                        Postgres.disconnect (PGDisconnectError commandId) (PGDisconnect commandId) connectionId False
                in
                    ( model ! [ cmd ], [ config.rollbackTagger commandId ] )

            RollbackError commandId ( connectionId, error ) ->
                let
                    cmd =
                        Postgres.disconnect (PGDisconnectError commandId) (PGDisconnect commandId) connectionId True

                    errMsg =
                        nonFatal commandId ("RollbackError:" +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
                in
                    ( model ! [], [ errMsg, config.rollbackErrorTagger ( commandId, error ) ] )

            WriteEvents commandId statement ( connectionId, results ) ->
                let
                    ( eventRows, errorMsg ) =
                        List.head results
                            |?> (\result ->
                                    JD.decodeString insertEventsResponseDecoder result
                                        |??> (\response -> ( response.insert_events, "" ))
                                        ??= (\err -> ( -1, "SQL insert_events Command results could not be decoded. Error:" +-+ err ))
                                )
                            ?!= (\_ -> ( -1, "SQL Insert Events Command results list is EMPTY" ))
                in
                    ( model ! []
                    , [ (errorMsg /= "") ? ( fatal commandId errorMsg, config.writeEventsTagger ( commandId, eventRows ) )
                      ]
                    )

            WriteEventsError commandId statement ( connectionId, error ) ->
                let
                    errMsg =
                        nonFatal commandId ("WriteEventsError:" +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
                in
                    ( model ! [], [ errMsg, config.writeEventsErrorTagger ( commandId, error ) ] )

            LockerError ( errorType, ( commandId, message ) ) ->
                let
                    errorHandler =
                        case errorType of
                            FatalError ->
                                fatal

                            NonFatalError ->
                                nonFatal

                            _ ->
                                Debug.crash "Unexpected error type"
                in
                    ( model ! [], [ errorHandler commandId message ] )

            LockerLog ( logLevel, ( commandId, message ) ) ->
                let
                    logHandler =
                        case logLevel of
                            LogLevelInfo ->
                                logMsg

                            _ ->
                                Debug.crash "Unexpected log level"
                in
                    ( model ! [], [ logHandler commandId message ] )

            LockerModule msg ->
                updateLocker msg model


{-|
    API
-}
initCommand : Config msg -> Model -> Result String ( Model, Cmd Msg )
initCommand config model =
    Ok
        ( { model | nextCommandId = model.nextCommandId + 1 }
        , connectCmd config model.nextCommandId 1
        )


lockEntities : Model -> CommandId -> List String -> Result String ( Model, Cmd Msg )
lockEntities model commandId entities =
    let
        maybeConnectionId =
            Dict.get commandId model.commandIds
    in
        case maybeConnectionId of
            Just connectionId ->
                let
                    ( lockerModel, cmd ) =
                        Locker.lock model.lockerModel commandId connectionId entities
                in
                    Ok ( { model | lockerModel = lockerModel }, Cmd.map LockerModule cmd )

            Nothing ->
                Err <| "CommandId:  " ++ (toString commandId) ++ " doesn't exist"


writeEvents : Model -> CommandId -> List String -> Result String ( Model, Cmd Msg )
writeEvents model commandId events =
    let
        maybeConnectionId =
            Dict.get commandId model.commandIds
    in
        case maybeConnectionId of
            Just connectionId ->
                let
                    cmd =
                        writeEventsCmd commandId connectionId events
                in
                    Ok ( model, cmd )

            Nothing ->
                Err <| "CommandId:  " ++ (toString commandId) ++ " doesn't exist"


commit : Model -> CommandId -> Result String ( Model, Cmd Msg )
commit model commandId =
    let
        maybeConnectionId =
            Dict.get commandId model.commandIds
    in
        case maybeConnectionId of
            Just connectionId ->
                let
                    cmd =
                        Postgres.query (CommitError commandId) (Commit commandId) connectionId "COMMIT" 1
                in
                    Ok ( model, cmd )

            Nothing ->
                Err <| "CommandId:  " ++ (toString commandId) ++ " doesn't exist"


rollback : Model -> CommandId -> Result String ( Model, Cmd Msg )
rollback model commandId =
    let
        maybeConnectionId =
            Dict.get commandId model.commandIds
    in
        case maybeConnectionId of
            Just connectionId ->
                let
                    cmd =
                        Postgres.query (RollbackError commandId) (Rollback commandId) connectionId "ROLLBACK" 1
                in
                    Ok ( model, cmd )

            Nothing ->
                Err <| "CommandId:  " ++ (toString commandId) ++ " doesn't exist"


createMetaData : String -> String -> Metadata
createMetaData initiatorId command =
    { initiatorId = initiatorId, command = command }


{-|

-}
writeEventsCmd : CommandId -> ConnectionId -> List String -> Cmd Msg
writeEventsCmd commandId connectionId events =
    let
        statement =
            insertEventsStatement events
    in
        Postgres.query (BeginError commandId statement) (Begin commandId statement) connectionId "BEGIN" 1


insertEventsStatement : List String -> String
insertEventsStatement events =
    let
        createEvents totalEvents event newEvents =
            let
                i =
                    totalEvents - List.length newEvents
            in
                "($1[" +++ i +++ "]," +++ "$2,'" +++ event +++ "')" :: newEvents

        newEventList =
            String.join "," <| List.foldr (createEvents <| List.length events) [] events
    in
        "SELECT insert_events($$" +++ newEventList +++ "$$)"


connectCmd : Config msg -> CommandId -> Int -> Cmd Msg
connectCmd config commandId retries =
    Postgres.connect (PGConnectError commandId retries)
        (PGConnect commandId)
        (PGConnectionLost commandId)
        config.pgConnectionConfig.connectTimeout
        config.pgConnectionConfig.host
        config.pgConnectionConfig.port_
        config.pgConnectionConfig.database
        config.pgConnectionConfig.user
        config.pgConnectionConfig.password
