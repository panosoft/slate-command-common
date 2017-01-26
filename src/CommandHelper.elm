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
        , writeEvents
        , update
        )

import Dict exposing (Dict)
import Json.Decode as JD exposing (..)
import Process
import Task exposing (Task)
import Time exposing (Time, second)
import String exposing (join)
import Postgres exposing (..)
import ParentChildUpdate exposing (..)
import StringUtils exposing ((+-+), (+++))
import Utils.Json exposing ((<||))
import Utils.Ops exposing (..)
import Locker exposing (..)


type alias CommandId =
    Int


type alias ConnectionId =
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


type alias Config msg =
    { pgConnectionInfo : PGConnectionInfo
    , pgReconnectDelayInterval : Time
    , errorTagger : ErrorTagger msg
    , logTagger : LogTagger msg
    , initCommandTagger : InitCommandTagger msg
    , initCommandErrorTagger : InitCommandErrorTagger msg
    , lockEntitiesTagger : LockEntitiesTagger msg
    , lockEntitiesErrorTagger : LockEntitiesErrorTagger msg
    , writeEventsTagger : WriteEventsTagger msg
    , writeEventsErrorTagger : WriteEventsErrorTagger msg
    }


lockerConfig : Locker.Config Msg
lockerConfig =
    { errorTagger = LockerError
    , logTagger = LockerLog
    , lockEntitiesTagger = LockEntities
    , lockEntitiesErrorTagger = LockEntitiesError
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
    | LockEntities CommandId
    | LockEntitiesError ( CommandId, String )
    | Begin CommandId String ( ConnectionId, List String )
    | BeginError CommandId String ( ConnectionId, String )
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

            PGConnectError commandId ( _, pgError ) ->
                ( model ! []
                , [ logErr ("initCommand Error:" +-+ "Command Id:" +-+ commandId +-+ "Connection Error:" +-+ pgError)
                  , config.initCommandErrorTagger ( commandId, pgError )
                  ]
                )

            PGConnectionLost commandId ( connectionId, error ) ->
                ( model ! []
                , [ logErr ("PGConnectLost:" +-+ "Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Connection Error:" +-+ error) ]
                )

            PGDisconnectError commandId ( connectionId, error ) ->
                ( model ! [], [] )

            PGDisconnect commandId connectionId ->
                ( model ! [], [] )

            LockEntities commandId ->
                ( model ! [], [ config.lockEntitiesTagger commandId ] )

            LockEntitiesError ( commandId, error ) ->
                ( model ! [], [ config.lockEntitiesErrorTagger ( commandId, error ) ] )

            Begin commandId statement ( connectionId, results ) ->
                let
                    cmd =
                        Postgres.query (WriteEventsError commandId statement) (WriteEvents commandId statement) connectionId statement 2
                in
                    ( model ! [ cmd ], [] )

            BeginError commandId statement ( connectionId, error ) ->
                ( model ! [], [ config.writeEventsErrorTagger ( commandId, error ) ] )

            WriteEvents commandId statement ( connectionId, results ) ->
                let
                    l =
                        ( Debug.log "CommandHelper WriteEvents"
                            ("Command Id :" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Results:" +-+ results)
                        , Debug.log "CommandHelper Model" model
                        )

                    eventRows =
                        List.head results
                            |?> (\result ->
                                    JD.decodeString insertEventsResponseDecoder result
                                        |??> (\response -> response.insert_events)
                                        ??= Debug.crash
                                )
                            ?!= (\_ -> Debug.crash "SQL Insert Events Command results list is empty")
                in
                    ( model ! [], [ config.writeEventsTagger ( commandId, eventRows ) ] )

            WriteEventsError commandId statement ( connectionId, error ) ->
                let
                    l =
                        Debug.log "CommandHelper WriteEventsError"
                            ("Command Id:" +-+ commandId +-+ "Connection Id:" +-+ connectionId +-+ "Connection Error:" +-+ error)
                in
                    ( model ! [], [ config.writeEventsErrorTagger ( commandId, error ) ] )

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


lockEntities : Config msg -> Model -> CommandId -> List String -> Result String ( Model, Cmd Msg )
lockEntities config model commandId entities =
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


writeEvents : Config msg -> Model -> CommandId -> List String -> Result String ( Model, Cmd Msg )
writeEvents config model commandId events =
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


{-|

-}
writeEventsCmd : CommandId -> ConnectionId -> List String -> Cmd Msg
writeEventsCmd commandId connectionId events =
    let
        statement =
            insertEventsStatement events

        l =
            Debug.log "Write Events SQL" statement
    in
        Postgres.query (BeginError commandId statement) (Begin commandId statement) connectionId "Begin" 1


insertEventsStatement : List String -> String
insertEventsStatement events =
    let
        createEvents event newEvents =
            let
                i =
                    List.length newEvents + 1
            in
                "($1[" +++ i +++ "]," +++ "$2,'" +++ event +++ "')" :: newEvents

        newEventList =
            String.join "," <| List.reverse <| List.foldl createEvents [] events
    in
        "SELECT insert_events($$" +++ newEventList +++ "$$)"
