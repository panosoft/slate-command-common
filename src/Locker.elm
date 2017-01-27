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
import Json.Decode as JD exposing (..)
import FNV exposing (..)
import Postgres exposing (..)
import Utils.Ops exposing (..)
import Utils.Json exposing ((<||))
import StringUtils exposing (..)
import List exposing (isEmpty)


beginTrans : String
beginTrans =
    "BEGIN"


type alias CommandId =
    Int


type alias Guid =
    String


type alias LockState =
    { guids : List Guid
    , locks : List Int
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
    | BeginCommand CommandId Int ( ConnectionId, List String )
    | BeginCommandError CommandId ( ConnectionId, String )
    | LockEntities CommandId Int ( ConnectionId, List String )
    | LockEntitiesError CommandId ( ConnectionId, String )
    | Rollback CommandId Int ( ConnectionId, List String )
    | RollbackError CommandId ( ConnectionId, String )


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

            BeginCommand commandId retries ( connectionId, results ) ->
                let
                    test =
                        List.isEmpty results
                            ?! ( (\_ -> ())
                               , (\_ ->
                                    Debug.crash
                                        ("SQL BEGIN Command results list is not empty for CommandId:"
                                            +-+ commandId
                                            +++ ". Results:"
                                            ++ (toString results)
                                        )
                                 )
                               )
                in
                    processNextLock config model commandId connectionId retries

            BeginCommandError commandId ( connectionId, error ) ->
                ( model ! [], [ logErr ("BeginCommandError  CommandId:" +-+ commandId +-+ "ConnectionId:" +-+ connectionId +-+ "SQL error:" +-+ error) ] )

            LockEntities commandId retries ( connectionId, results ) ->
                didLock results commandId
                    ? ( processNextLock config model commandId connectionId retries, retryLocks config model commandId connectionId retries )

            LockEntitiesError commandId ( connectionId, error ) ->
                let
                    errMsg =
                        "Locker LockEntitiesError   CommandId:" +-+ commandId +-+ "ConnectionId:" +-+ connectionId +-+ "SQL error:" +-+ error
                in
                    ( model ! []
                    , [ logErr errMsg
                      , config.lockEntitiesErrorTagger ( commandId, errMsg )
                      ]
                    )

            Rollback commandId retries ( connectionId, results ) ->
                let
                    test =
                        List.isEmpty results
                            ?! ( (\_ -> ())
                               , (\_ ->
                                    Debug.crash
                                        ("SQL ROLLBACK Command results list is not empty for CommandId:"
                                            +-+ commandId
                                            +++ ". Results:"
                                            ++ (toString results)
                                        )
                                 )
                               )

                    ( cmd, parentMsgs ) =
                        (retries < 1)
                            ? ( ( Cmd.none
                                , [ config.lockEntitiesErrorTagger ( commandId, "Failed to obtain all locks for CommandId:" +-+ commandId ) ]
                                )
                              , ( Postgres.query (BeginCommandError commandId) (BeginCommand commandId (retries - 1)) connectionId beginTrans 1
                                , [ logErr ("lock Command Error:" +-+ "Command Id:" +-+ commandId +-+ "Error:" +-+ "Could not obtains all locks." +-+ "Retries remaining:" +-+ retries) ]
                                )
                              )
                in
                    ( model ! [ cmd ], parentMsgs )

            RollbackError commandId ( connectionId, error ) ->
                let
                    parentMsgs =
                        [ logErr ("Locker RollbackError   CommandId:" +-+ commandId +-+ "ConnectionId:" +-+ connectionId +-+ "SQL error:" +-+ error) ]
                in
                    ( model ! [], parentMsgs )


{-|
    API
-}
lock : Model -> CommandId -> ConnectionId -> List Guid -> Int -> ( Model, Cmd Msg )
lock model commandId connectionId guids retries =
    let
        locks =
            createLocks guids

        lockRequests =
            Dict.insert connectionId (LockState guids locks) model.lockRequests
    in
        ( { model | lockRequests = lockRequests }
        , Postgres.query (BeginCommandError commandId) (BeginCommand commandId retries) connectionId beginTrans 1
        )


{-|
    Helpers
-}
createLocks : List Guid -> List Int
createLocks guids =
    List.map (\guid -> FNV.hashString guid) guids


processNextLock : Config msg -> Model -> CommandId -> ConnectionId -> Int -> ( ( Model, Cmd Msg ), List msg )
processNextLock config model commandId connectionId retries =
    Dict.get connectionId model.lockRequests
        |?> (\lockState ->
                List.head lockState.locks
                    |?> (\lock ->
                            ( { model | lockRequests = Dict.insert connectionId { lockState | locks = List.drop 1 lockState.locks } model.lockRequests }
                                ! [ lockCmd commandId connectionId lock retries ]
                            , []
                            )
                        )
                    ?= ( { model | lockRequests = Dict.remove connectionId model.lockRequests } ! [], [ config.lockEntitiesTagger commandId ] )
            )
        ?!= (\_ -> Debug.crash ("BUG -- Missing connectionId for CommandId:" +-+ commandId))


retryLocks : Config msg -> Model -> CommandId -> ConnectionId -> Int -> ( ( Model, Cmd Msg ), List msg )
retryLocks config model commandId connectionId retries =
    Dict.get connectionId model.lockRequests
        |?> (\lockState ->
                ( { model | lockRequests = Dict.insert connectionId { lockState | locks = createLocks lockState.guids } model.lockRequests }
                    ! [ Postgres.query (RollbackError commandId) (Rollback commandId retries) connectionId "ROLLBACK" 1 ]
                , []
                )
            )
        ?!= (\_ -> Debug.crash ("BUG -- Missing connectionId for CommandId:" +-+ commandId))


lockCmd : CommandId -> ConnectionId -> Int -> Int -> Cmd Msg
lockCmd commandId connectionId lock retries =
    let
        createLockStatement lock =
            "SELECT pg_try_advisory_xact_lock(" ++ (toString lock) ++ ");"
    in
        Postgres.query (LockEntitiesError commandId) (LockEntities commandId retries) connectionId (createLockStatement lock) 2


didLock : List String -> CommandId -> Bool
didLock results commandId =
    List.head results
        |?> (\result ->
                JD.decodeString lockResponseDecoder result
                    |??> (\response -> response.pg_try_advisory_xact_lock)
                    ??= (\err -> Debug.crash ("SQL Lock Command results could not be decoded for CommandId:" +-+ commandId +++ ". Error:" +-+ err))
            )
        ?!= (\_ -> Debug.crash ("SQL Lock Command results list is empty for CommandId:" +-+ commandId))
