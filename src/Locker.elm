module Locker
    exposing
        ( Config
        , Model
        , Msg
        , init
        , update
        , lock
        )

import Dict exposing (..)


-- import String exposing (reverse)

import Json.Decode as JD exposing (..)
import FNV exposing (..)
import Postgres exposing (..)
import Utils.Ops exposing (..)
import Utils.Json exposing ((<||))
import StringUtils exposing (..)


beginTrans : String
beginTrans =
    "BEGIN"


tryAdvisoryLock : String
tryAdvisoryLock =
    "pg_try_advisory_xact_lock"


type alias CommandId =
    Int


type alias Guid =
    String


type alias LockState =
    { guids : List Guid
    , locks : List Int
    , retryCount : Int
    }


type alias LockDict =
    Dict ConnectionId LockState


type alias LockResponse =
    { pg_try_advisory_xact_lock : Bool
    }


lockResponseDecoder : JD.Decoder LockResponse
lockResponseDecoder =
    JD.succeed LockResponse
        <|| ("pg_try_advisory_xact_lock" := bool)


{-|
parent msg taggers
-}
type alias ErrorTagger msg =
    String -> msg


type alias LogTagger msg =
    String -> msg


type alias LockEntitiesTagger msg =
    CommandId -> msg


type alias LockEntitiesErrorTagger msg =
    ( CommandId, String ) -> msg


type alias Config msg =
    { errorTagger : ErrorTagger msg
    , logTagger : LogTagger msg
    , lockEntitiesTagger : LockEntitiesTagger msg
    , lockEntitiesErrorTagger : LockEntitiesErrorTagger msg
    }


type Msg
    = Nop
    | BeginCommand CommandId ( ConnectionId, List String )
    | BeginCommandError CommandId ( ConnectionId, String )
    | LockEntities CommandId ( ConnectionId, List String )
    | LockEntitiesError CommandId ( ConnectionId, String )
    | RollbackComplete CommandId ( ConnectionId, List String )


type alias Model =
    { lockRequests : LockDict
    }


init : ( Model, Cmd Msg )
init =
    ({ lockRequests = Dict.empty } ! [])


update : Config msg -> Msg -> Model -> ( ( Model, Cmd Msg ), List msg )
update config msg model =
    let
        logMsg message =
            config.logTagger message

        logErr error =
            config.errorTagger error
    in
        case msg of
            Nop ->
                ( model ! [], [] )

            BeginCommand commandId ( connectionId, results ) ->
                let
                    l =
                        ( Debug.log "Locker BeginCommand" ("CommandId:" +-+ commandId +-+ "ConnectionId:" +-+ connectionId +-+ "SQL Response:" +-+ results)
                        , Debug.log "Locker Model" (toString model)
                        )
                in
                    processNextLock config model commandId connectionId

            BeginCommandError commandId ( connectionId, error ) ->
                let
                    l =
                        ( Debug.log "Locker BeginCommandError" ("CommandId:" +-+ commandId +-+ "ConnectionId:" +-+ connectionId +-+ "SQL error:" +-+ error)
                        , Debug.log "Locker Model" (toString model)
                        )
                in
                    ( model ! [], [] )

            LockEntities commandId ( connectionId, results ) ->
                let
                    l =
                        ( Debug.log "Locker LockEntities" ("CommandId:" +-+ commandId +-+ "ConnectionId:" +-+ connectionId +-+ "SQL Response:" +-+ results)
                        , Debug.log "Locker Model" (toString model)
                        )
                in
                    didLock results
                        ? ( processNextLock config model commandId connectionId
                            -- TODO rollback and retry from the beginning
                          , ( model ! [], [] )
                          )

            LockEntitiesError commandId ( connectionId, error ) ->
                let
                    l =
                        ( Debug.log "Locker LockEntitiesError" ("CommandId:" +-+ commandId +-+ "ConnectionId:" +-+ connectionId +-+ "SQL error:" +-+ error)
                        , Debug.log "Locker Model" (toString model)
                        )
                in
                    ( model ! [], [] )

            RollbackComplete commandId ( connectionId, results ) ->
                ( model ! [], [] )


{-|
    API
-}
lock : Model -> CommandId -> ConnectionId -> List Guid -> ( Model, Cmd Msg )
lock model commandId connectionId guids =
    let
        locks =
            createLocks guids

        lockRequests =
            Dict.insert connectionId (LockState guids locks 0) model.lockRequests
    in
        ( { model | lockRequests = lockRequests }
        , Postgres.query (BeginCommandError commandId) (BeginCommand commandId) connectionId beginTrans 1
        )


{-|
    Helpers
-}
createLocks : List Guid -> List Int
createLocks guids =
    List.map (\guid -> FNV.hashString guid) guids


processNextLock : Config msg -> Model -> CommandId -> ConnectionId -> ( ( Model, Cmd Msg ), List msg )
processNextLock config model commandId connectionId =
    Dict.get connectionId model.lockRequests
        |?> (\lockState ->
                List.head lockState.locks
                    |?> (\lock ->
                            ( { model | lockRequests = Dict.insert connectionId { lockState | locks = List.drop 1 lockState.locks } model.lockRequests }
                                ! [ lockCmd commandId connectionId lock ]
                            , []
                            )
                        )
                    -- TODO
                    ?=
                        ( { model | lockRequests = Dict.remove connectionId model.lockRequests } ! [], [ config.lockEntitiesTagger commandId ] )
            )
        ?!= (\_ -> Debug.crash "BUG -- Missing connectionId")


lockCmd : CommandId -> ConnectionId -> Int -> Cmd Msg
lockCmd commandId connectionId lock =
    let
        createLockStatement lock =
            "SELECT " ++ tryAdvisoryLock ++ "(" ++ (toString lock) ++ ");"
    in
        Postgres.query (LockEntitiesError commandId) (LockEntities commandId) connectionId (createLockStatement lock) 2


didLock : List String -> Bool
didLock results =
    List.head results
        |?> (\result ->
                JD.decodeString lockResponseDecoder result
                    |??> (\response -> response.pg_try_advisory_xact_lock)
                    ??= Debug.crash
            )
        ?!= (\_ -> Debug.crash "SQL Lock Command results list is empty")
