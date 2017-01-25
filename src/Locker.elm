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


beginTrans : String
beginTrans =
    "BEGIN"


tryAdvisoryLock : String
tryAdvisoryLock =
    "pg_try_advisory_xact_lock"


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


type alias Config msg =
    { errorTagger : ErrorTagger msg
    , logTagger : LogTagger msg
    }


type Msg
    = Nop
    | BeginCommandSuccess ( ConnectionId, List String )
    | BeginCommandFailure ( ConnectionId, String )
    | LockCommandSuccess ( ConnectionId, List String )
    | LockCommandFailure ( ConnectionId, String )
    | RollbackComplete ( ConnectionId, List String )


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

            BeginCommandSuccess ( connectionId, results ) ->
                let
                    l =
                        Debug.log "BeginCommandSuccess" results
                in
                    processNextLock model connectionId

            BeginCommandFailure ( connectionId, error ) ->
                let
                    l =
                        Debug.log "BeginCommandFailure" error
                in
                    ( model ! [], [] )

            LockCommandSuccess ( connectionId, results ) ->
                let
                    l =
                        Debug.log "LockCommandSuccess" results
                in
                    didLock results
                        ? ( processNextLock model connectionId
                            -- TODO rollback and retry from the beginning
                          , ( model ! [], [] )
                          )

            -- l =
            --     ( Debug.log "Lock Statement" lockStatement, Debug.log "New Model" (toString newModel), Debug.log "Decoded results" (toString lockResult) )
            -- ll =
            --     ( Debug.log "LockCommandSuccess" results, Debug.log "Current model" (toString model) )
            LockCommandFailure ( connectionId, error ) ->
                let
                    l =
                        Debug.log "LockCommandFailure" error
                in
                    ( model ! [], [] )

            RollbackComplete ( connectionId, results ) ->
                ( model ! [], [] )


{-|
    API
-}
lock : Model -> ConnectionId -> List Guid -> ( Model, Cmd Msg )
lock model connectionId guids =
    let
        locks =
            createLocks guids

        lockRequests =
            Dict.insert connectionId (LockState guids locks 0) model.lockRequests
    in
        ( { model | lockRequests = lockRequests }
        , Postgres.query BeginCommandFailure BeginCommandSuccess connectionId beginTrans 1000
        )


{-|
    Helpers
-}
createLocks : List Guid -> List Int
createLocks guids =
    List.map (\guid -> FNV.hashString guid) guids


processNextLock : Model -> ConnectionId -> ( ( Model, Cmd Msg ), List msg )
processNextLock model connectionId =
    Dict.get connectionId model.lockRequests
        |?> (\lockState ->
                List.head lockState.locks
                    |?> (\lock ->
                            ( { model | lockRequests = Dict.insert connectionId { lockState | locks = List.drop 1 lockState.locks } model.lockRequests }
                                ! [ lockCmd connectionId lock ]
                            , []
                            )
                        )
                    -- TODO
                    ?=
                        ( model ! [], [] )
            )
        ?!= (\_ -> Debug.crash "BUG -- Missing connectionId")


lockCmd : ConnectionId -> Int -> Cmd Msg
lockCmd connectionId lock =
    let
        createLockStatement lock =
            "SELECT " ++ tryAdvisoryLock ++ "(" ++ (toString lock) ++ ");"
    in
        Postgres.query LockCommandFailure LockCommandSuccess connectionId (createLockStatement lock) 2


didLock : List String -> Bool
didLock results =
    List.head results
        |?> (\result ->
                JD.decodeString lockResponseDecoder result
                    |??> (\response -> response.pg_try_advisory_xact_lock)
                    ??= Debug.crash
            )
        ?!= (\_ -> Debug.crash "SQL Lock Command results list is empty")
