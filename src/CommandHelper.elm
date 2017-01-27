module CommandHelper
    exposing
        ( Msg
        , Model
        , Config
        , CommandId
        , PGConnectionInfo
        , Metadata
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
import Locker exposing (..)


type alias Metadata =
    { initiatorId : String
    , command : String
    }


type alias CommandId =
    Int


type alias ConnectionId =
    Int


type alias CommandIdDict =
    Dict CommandId ConnectionId


type alias InsertEventsResponse =
    { insert_events : Int
    }


lockRetries : Int
lockRetries =
    3


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
    Postgres connection info
-}
type alias PGConnectionInfo =
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
    String -> msg


type alias LogTagger msg =
    String -> msg


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


type alias Config msg =
    { pgConnectionInfo : PGConnectionInfo
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
    }


lockerConfig : Locker.Config Msg
lockerConfig =
    { errorTagger = LockerError
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

            PGConnect commandId connectionId ->
                let
                    commandIds =
                        Dict.insert commandId connectionId model.commandIds
                in
                    ( { model | commandIds = commandIds } ! []
                    , [ logMsg ("PGConnect:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId)
                      , config.initCommandTagger commandId
                      ]
                    )

            PGConnectError commandId retries ( _, error ) ->
                let
                    newRetries =
                        retries - 1

                    errMsg =
                        logErr ("initCommand Error:" +-+ "Command Id:" +-+ commandId +-+ "Connection Error:" +-+ error +-+ "Connection Retries remaining:" +-+ retries)

                    ( cmd, appMsgs ) =
                        (newRetries < 1)
                            ? ( ( Cmd.none, [ errMsg, config.initCommandErrorTagger ( commandId, error ) ] )
                              , ( delayCmd (connectCmd commandId config.pgConnectionInfo newRetries) config.pgConnectionInfo.reconnectDelayInterval
                                , [ errMsg ]
                                )
                              )
                in
                    ( model ! [ cmd ], appMsgs )

            PGConnectionLost commandId ( connectionId, error ) ->
                ( model ! []
                , [ logErr ("PGConnectLost:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Connection Error:" +-+ error) ]
                )

            PGDisconnectError commandId ( connectionId, error ) ->
                let
                    -- l =
                    --     Debug.log "CommandHelper PGDisconnectError"
                    --         ("Command Id :" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error +-+ "CommandIds:" +-+ commandIds)
                    commandIds =
                        Dict.remove commandId model.commandIds

                    parentMsgs =
                        [ logErr ("PGDisconnectError:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Connection Error:" +-+ error) ]
                in
                    ( { model | commandIds = commandIds } ! [], parentMsgs )

            PGDisconnect commandId connectionId ->
                let
                    -- l =
                    --     Debug.log "CommandHelper PGDisconnect"
                    --         ("Command Id :" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "CommandIds:" +-+ commandIds)
                    commandIds =
                        Dict.remove commandId model.commandIds
                in
                    ( { model | commandIds = commandIds } ! [], [] )

            LockEntities commandId ->
                ( model ! [], [ config.lockEntitiesTagger commandId ] )

            LockEntitiesError ( commandId, error ) ->
                let
                    errMsg =
                        logErr ("LockEntitiesError:" +-+ "Command Id:" +-+ commandId +-+ "Error:" +-+ error)
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
                        logErr ("BeginError:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
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
                        logErr ("CommitError:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
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
                        logErr ("RollbackError:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
                in
                    ( model ! [], [ errMsg, config.rollbackErrorTagger ( commandId, error ) ] )

            WriteEvents commandId statement ( connectionId, results ) ->
                let
                    -- l =
                    --     ( Debug.log "CommandHelper WriteEvents"
                    --         ("Command Id :" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Results:" +-+ results)
                    --     , Debug.log "CommandHelper Model" model
                    --     )
                    eventRows =
                        List.head results
                            |?> (\result ->
                                    JD.decodeString insertEventsResponseDecoder result
                                        |??> (\response -> response.insert_events)
                                        ??= (\err -> Debug.crash ("SQL insert_events Command results could not be decoded for CommandId:" +-+ commandId +++ ". Error:" +-+ err))
                                )
                            ?!= (\_ -> Debug.crash ("SQL Insert Events Command results list is  for CommandId:" +-+ commandId))
                in
                    ( model ! [], [ config.writeEventsTagger ( commandId, eventRows ) ] )

            WriteEventsError commandId statement ( connectionId, error ) ->
                let
                    --     l =
                    --         Debug.log "CommandHelper WriteEventsError"
                    --             ("Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Connection Error:" +-+ error)
                    errMsg =
                        logErr ("WriteEventsError:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Error:" +-+ error)
                in
                    ( model ! [], [ errMsg, config.writeEventsErrorTagger ( commandId, error ) ] )

            LockerError message ->
                ( model ! [], [ config.errorTagger message ] )

            LockerLog message ->
                ( model ! [], [ config.logTagger message ] )

            LockerModule msg ->
                updateLocker msg model


{-|
    API
-}
initCommand : Model -> PGConnectionInfo -> Result String ( Model, Cmd Msg )
initCommand model pgConnectionInfo =
    Ok
        ( { model | nextCommandId = model.nextCommandId + 1 }
        , connectCmd model.nextCommandId pgConnectionInfo pgConnectionInfo.retries
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
                        Locker.lock model.lockerModel commandId connectionId entities lockRetries
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

        -- l =
        --     Debug.log "Write Events SQL" statement
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


connectCmd : CommandId -> PGConnectionInfo -> Int -> Cmd Msg
connectCmd commandId pgConnectionInfo retries =
    Postgres.connect (PGConnectError commandId retries)
        (PGConnect commandId)
        (PGConnectionLost commandId)
        pgConnectionInfo.connectTimeout
        pgConnectionInfo.host
        pgConnectionInfo.port_
        pgConnectionInfo.database
        pgConnectionInfo.user
        pgConnectionInfo.password
