module Slate.Command.Helper
    exposing
        ( Msg
        , Model
        , Config
        , init
        , update
        , initCommand
        , lockEntities
        , writeEvents
        , commit
        , rollback
        )

{-|
    Helper functions for writing Slate Entity APIs.

@docs Msg , Model , Config   , init , update , initCommand , lockEntities , writeEvents , commit , rollback
-}

import Set
import Time exposing (Time)
import Dict exposing (Dict)
import Json.Decode as JD exposing (..)
import String exposing (join)
import StringUtils exposing ((+-+), (+++))
import Slate.Command.Locker as Locker exposing (..)
import Slate.Command.Common.Command exposing (..)
import Postgres exposing (..)
import ParentChildUpdate exposing (..)
import Utils.Json exposing ((<||))
import Utils.Ops exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import Retry exposing (..)
import Slate.Common.Db exposing (..)
import Services.Common.Taggers exposing (..)


type alias CommandIdDict =
    Dict CommandId ConnectionId


type alias InsertEventsResponse =
    { insert_events : Int
    }


{-|
    parent msg taggers
-}
type alias RouteToMeTagger msg =
    Msg -> msg


{-|
    Tagger for parent indicating initCommand succeeded.
-}
type alias InitCommandTagger msg =
    CommandId -> msg


{-|
    Tagger for parent indicating initCommand had error.
-}
type alias InitCommandErrorTagger msg =
    ( CommandId, String ) -> msg


{-|
    Tagger for parent indicating lockEntities succeeded.
-}
type alias LockEntitiesTagger msg =
    CommandId -> msg


{-|
    Tagger for parent indicating lockEntities had error.
-}
type alias LockEntitiesErrorTagger msg =
    ( CommandId, String ) -> msg


{-|
    Tagger for parent indicating writeEvents succeeded.
-}
type alias WriteEventsTagger msg =
    ( CommandId, Int ) -> msg


{-|
    Tagger for parent indicating writeEvents had error.
-}
type alias WriteEventsErrorTagger msg =
    ( CommandId, String ) -> msg


{-|
    Tagger for parent indicating commit succeeded.
-}
type alias CommitTagger msg =
    CommandId -> msg


{-|
    Tagger for parent indicating commit had error.
-}
type alias CommitErrorTagger msg =
    ( CommandId, String ) -> msg


{-|
    Tagger for parent indicating rollback succeeded.
-}
type alias RollbackTagger msg =
    String -> CommandId -> msg


{-|
    Tagger for parent indicating rollback had error.
-}
type alias RollbackErrorTagger msg =
    String -> ( CommandId, String ) -> msg


{-|
    Tagger for parent indicating database connection was closed unexpectedly.
-}
type alias ConnectionLostTagger msg =
    ( CommandId, String ) -> msg


{-|
    CommandHelper Config.
-}
type alias Config msg =
    { retryMax : Maybe Int
    , delayNext : Maybe (Int -> Time)
    , lockRetries : Maybe Int
    , routeToMeTagger : RouteToMeTagger msg
    , errorTagger : ErrorTagger ( CommandId, String ) msg
    , logTagger : LogTagger ( CommandId, String ) msg
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
    { retries = config.lockRetries ?= 3
    , lockerTagger = LockerModule
    , errorTagger = LockerError
    , logTagger = LockerLog
    , lockEntitiesTagger = LockEntities
    , lockEntitiesErrorTagger = LockEntitiesError
    }


retryConfig : Config msg -> Retry.Config Msg
retryConfig config =
    { retryMax = config.retryMax ?= 3
    , delayNext = config.delayNext ?= Retry.constantDelay 5000
    , routeToMeTagger = RetryModule
    }


{-|
    CommandHelper Msgs
-}
type Msg
    = Nop
    | PGConnect CommandId ConnectionId
    | PGConnectError CommandId ( ConnectionId, String )
    | PGConnectionLost CommandId ( ConnectionId, String )
    | PGDisconnectError CommandId ( ConnectionId, String )
    | PGDisconnect CommandId ConnectionId
    | LockEntities CommandId
    | LockEntitiesError ( CommandId, String )
    | Begin CommandId String ( ConnectionId, List String )
    | BeginError CommandId String ( ConnectionId, String )
    | Commit CommandId ( ConnectionId, List String )
    | CommitError CommandId ( ConnectionId, String )
    | Rollback String CommandId ( ConnectionId, List String )
    | RollbackError String CommandId ( ConnectionId, String )
    | WriteEvents CommandId String ( ConnectionId, List String )
    | WriteEventsError CommandId String ( ConnectionId, String )
    | LockerError ( ErrorType, ( CommandId, String ) )
    | LockerLog ( LogLevel, ( CommandId, String ) )
    | LockerModule Locker.Msg
    | RetryConnectCmd Int Msg (Cmd Msg)
    | RetryModule (Retry.Msg Msg)


{-|
    CommandHelper Config.
-}
type alias Model =
    { commandIds : CommandIdDict
    , nextCommandId : CommandId
    , lockerModel : Locker.Model
    , retryModel : Retry.Model Msg
    }


initModel : ( Model, List (Cmd Msg) )
initModel =
    let
        ( lockerModel, lockerCmd ) =
            Locker.init LockerModule
    in
        ( { commandIds = Dict.empty
          , nextCommandId = 0
          , lockerModel = lockerModel
          , retryModel = Retry.initModel
          }
        , [ lockerCmd ]
        )


{-|
    initialize command helper
-}
init : Config msg -> ( Model, Cmd msg )
init config =
    let
        ( model, cmds ) =
            initModel
    in
        model ! (List.map (Cmd.map config.routeToMeTagger) cmds)


insertEventsResponseDecoder : JD.Decoder InsertEventsResponse
insertEventsResponseDecoder =
    JD.succeed InsertEventsResponse
        <|| (field "insert_events" int)


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

        retryCfg =
            retryConfig config

        updateRetry =
            ParentChildUpdate.updateChildParent (Retry.update retryCfg) (update config) .retryModel retryCfg.routeToMeTagger (\model retryModel -> { model | retryModel = retryModel })
    in
        case msg of
            Nop ->
                ( model ! [], [] )

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

            PGConnectError commandId ( _, error ) ->
                ( model ! [], [ config.initCommandErrorTagger ( commandId, error ) ] )

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

            Rollback originalError commandId ( connectionId, results ) ->
                let
                    cmd =
                        Postgres.disconnect (PGDisconnectError commandId) (PGDisconnect commandId) connectionId False
                in
                    ( model ! [ cmd ], [ config.rollbackTagger originalError commandId ] )

            RollbackError originalError commandId ( connectionId, error ) ->
                let
                    cmd =
                        Postgres.disconnect (PGDisconnectError commandId) (PGDisconnect commandId) connectionId True

                    errMsg =
                        nonFatal commandId ("RollbackError:" +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
                in
                    ( model ! [], [ errMsg, config.rollbackErrorTagger originalError ( commandId, error ) ] )

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

            RetryConnectCmd retryCount failureMsg cmd ->
                let
                    parentMsg =
                        case failureMsg of
                            PGConnectError commandId ( _, error ) ->
                                nonFatal commandId ("initCommand Error:" +-+ "Connection Error:" +-+ error +-+ "Connection Retry:" +-+ retryCount)

                            _ ->
                                Debug.crash "BUG -- Should never get here"
                in
                    ( model ! [ cmd ], [ parentMsg ] )

            RetryModule msg ->
                updateRetry msg model



-- API


{-|
    initCommand
-}
initCommand : Config msg -> DbConnectionInfo -> Model -> ( Model, Cmd msg, Int )
initCommand config dbConnectionInfo model =
    let
        commandId =
            model.nextCommandId

        ( retryModel, retryCmd ) =
            Retry.retry (retryConfig config) model.retryModel (PGConnectError commandId) RetryConnectCmd (connectCmd dbConnectionInfo commandId)
    in
        ( { model | retryModel = retryModel, nextCommandId = model.nextCommandId + 1 }
        , Cmd.map config.routeToMeTagger <| retryCmd
        , commandId
        )


{-|
    lockEntities
-}
lockEntities : Config msg -> Model -> CommandId -> List String -> Result String ( Model, Cmd msg )
lockEntities config model commandId entityIds =
    let
        lock connectionId =
            let
                -- Sort entities to fail earlier and remove dups
                ( lockerModel, cmd ) =
                    Locker.lock (lockerConfig config) model.lockerModel commandId connectionId ((Set.toList << Set.fromList) entityIds)
            in
                ( { model | lockerModel = lockerModel }, Cmd.map config.routeToMeTagger cmd )
    in
        Dict.get commandId model.commandIds
            |?> (\connectionId -> Ok <| lock connectionId)
            ?= badCommandId commandId


{-|
    writeEvents
-}
writeEvents : Config msg -> Model -> CommandId -> List String -> Result String ( Model, Cmd msg )
writeEvents config model commandId events =
    let
        writeEventsCmd commandId connectionId events =
            let
                statement =
                    insertEventsStatement events
            in
                Cmd.map config.routeToMeTagger <| Postgres.query (BeginError commandId statement) (Begin commandId statement) connectionId "BEGIN" 1
    in
        Dict.get commandId model.commandIds
            |?> (\connectionId -> Ok ( model, (writeEventsCmd commandId connectionId events) ))
            ?= badCommandId commandId


{-|
    commit
-}
commit : Config msg -> Model -> CommandId -> Result String ( Model, Cmd msg )
commit config model commandId =
    Dict.get commandId model.commandIds
        |?> (\connectionId -> Ok ( model, (Cmd.map config.routeToMeTagger <| Postgres.query (CommitError commandId) (Commit commandId) connectionId "COMMIT" 1) ))
        ?= badCommandId commandId


{-|
    rollback
-}
rollback : Config msg -> Model -> CommandId -> String -> Result String ( Model, Cmd msg )
rollback config model commandId originalError =
    Dict.get commandId model.commandIds
        |?> (\connectionId -> Ok ( model, (Cmd.map config.routeToMeTagger <| Postgres.query (RollbackError originalError commandId) (Rollback originalError commandId) connectionId "ROLLBACK" 1) ))
        ?= badCommandId commandId


{-|
    Helpers
-}
badCommandId : CommandId -> Result String x
badCommandId commandId =
    Err <| "CommandId:" +-+ commandId +-+ "doesn't exist"


insertEventsStatement : List String -> String
insertEventsStatement events =
    let
        createEvents event newEvents =
            let
                i =
                    List.length events - List.length newEvents
            in
                "($1[" +++ i +++ "]," +++ "$2,'" +++ event +++ "')" :: newEvents

        newEventList =
            events
                |> List.foldr createEvents []
                |> String.join ","
    in
        "SELECT insert_events($$" +++ newEventList +++ "$$)"


connectCmd : DbConnectionInfo -> CommandId -> FailureTagger ( ConnectionId, String ) Msg -> Cmd Msg
connectCmd dbConnectionInfo commandId failureTagger =
    Postgres.connect failureTagger
        (PGConnect commandId)
        (PGConnectionLost commandId)
        dbConnectionInfo.timeout
        dbConnectionInfo.host
        dbConnectionInfo.port_
        dbConnectionInfo.database
        dbConnectionInfo.user
        dbConnectionInfo.password
