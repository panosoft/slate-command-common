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
import Utils.Error exposing (..)
import Utils.Log exposing (..)
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
    ( ErrorType, ( CommandId, String ) ) -> msg


type alias LogTagger msg =
    ( LogLevel, ( CommandId, String ) ) -> msg


type alias LockEntitiesTagger msg =
    CommandId -> msg


type alias LockEntitiesErrorTagger msg =
    ( CommandId, String ) -> msg


type alias Config msg =
    { retries : Int
    , errorTagger : ErrorTagger msg
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
        logMsg commandId message =
            config.logTagger ( LogLevelInfo, ( commandId, message ) )

        nonFatal commandId error =
            config.errorTagger ( NonFatalError, ( commandId, error ) )

        fatal commandId error =
            config.errorTagger ( FatalError, ( commandId, error ) )
    in
        case msg of
            Nop ->
                ( model ! [], [] )

            BeginCommand commandId retryCount ( connectionId, results ) ->
                let
                    fatalParentMsgs =
                        (results == [])
                            ?! ( (\_ -> [])
                               , (\_ -> [ fatal commandId ("SQL BEGIN Command results list is not empty. Results:" ++ (toString results)) ])
                               )
                in
                    (fatalParentMsgs /= [])
                        ? ( ( model ! [], fatalParentMsgs )
                          , processNextLock config model commandId connectionId retryCount
                          )

            BeginCommandError commandId ( connectionId, error ) ->
                ( model ! [], [ nonFatal commandId ("BeginCommandError:" +-+ "ConnectionId:" +-+ connectionId +-+ "SQL error:" +-+ error) ] )

            LockEntities commandId retryCount ( connectionId, results ) ->
                let
                    ( gotLock, fatalParentMsgs ) =
                        didLock config results commandId
                in
                    case fatalParentMsgs /= [] of
                        True ->
                            ( model ! [], fatalParentMsgs )

                        False ->
                            gotLock ? ( processNextLock config model commandId connectionId retryCount, retryLocks config model commandId connectionId retryCount )

            LockEntitiesError commandId ( connectionId, error ) ->
                let
                    errMsg =
                        "Locker LockEntitiesError:" +-+ "ConnectionId:" +-+ connectionId +-+ "SQL error:" +-+ error
                in
                    ( model ! []
                    , [ nonFatal commandId errMsg
                      , config.lockEntitiesErrorTagger ( commandId, errMsg )
                      ]
                    )

            Rollback commandId retryCount ( connectionId, results ) ->
                let
                    fatalParentMsgs =
                        (results == [])
                            ?! ( (\_ -> [])
                               , (\_ -> [ fatal commandId ("SQL ROLLBACK Command results list is not empty. Results:" ++ (toString results)) ])
                               )

                    ( cmd, parentMsgs ) =
                        (retryCount <= config.retries)
                            ? ( ( Postgres.query (BeginCommandError commandId) (BeginCommand commandId (retryCount + 1)) connectionId beginTrans 1
                                , [ nonFatal commandId ("lock Command Error:" +-+ "Error:" +-+ "Could not obtains all locks." +-+ "Retry:" +-+ retryCount) ]
                                )
                              , ( Cmd.none
                                , [ config.lockEntitiesErrorTagger ( commandId, "Failed to obtain all locks" ) ]
                                )
                              )
                in
                    (fatalParentMsgs /= [])
                        ? ( ( model ! [], fatalParentMsgs )
                          , ( model ! [ cmd ], parentMsgs )
                          )

            RollbackError commandId ( connectionId, error ) ->
                let
                    parentMsgs =
                        [ nonFatal commandId ("Locker RollbackError :" +-+ "ConnectionId:" +-+ connectionId +-+ "SQL error:" +-+ error) ]
                in
                    ( model ! [], parentMsgs )


{-|
    API
-}
lock : Model -> CommandId -> ConnectionId -> List Guid -> ( Model, Cmd Msg )
lock model commandId connectionId guids =
    let
        locks =
            createLocks guids

        lockRequests =
            Dict.insert connectionId (LockState guids locks) model.lockRequests
    in
        ( { model | lockRequests = lockRequests }
        , Postgres.query (BeginCommandError commandId) (BeginCommand commandId 1) connectionId beginTrans 1
        )


{-|
    Helpers
-}
createLocks : List Guid -> List Int
createLocks guids =
    List.map (\guid -> FNV.hashString guid) guids


processNextLock : Config msg -> Model -> CommandId -> ConnectionId -> Int -> ( ( Model, Cmd Msg ), List msg )
processNextLock config model commandId connectionId retryCount =
    Dict.get connectionId model.lockRequests
        |?> (\lockState ->
                List.head lockState.locks
                    |?> (\lock ->
                            ( { model | lockRequests = Dict.insert connectionId { lockState | locks = List.drop 1 lockState.locks } model.lockRequests }
                                ! [ lockCmd commandId connectionId lock retryCount ]
                            , []
                            )
                        )
                    ?= ( { model | lockRequests = Dict.remove connectionId model.lockRequests } ! [], [ config.lockEntitiesTagger commandId ] )
            )
        ?!= (\_ -> ( model ! [], [ config.errorTagger ( FatalError, ( commandId, ("BUG -- Missing connectionId") ) ) ] ))


retryLocks : Config msg -> Model -> CommandId -> ConnectionId -> Int -> ( ( Model, Cmd Msg ), List msg )
retryLocks config model commandId connectionId retryCount =
    Dict.get connectionId model.lockRequests
        |?> (\lockState ->
                ( { model | lockRequests = Dict.insert connectionId { lockState | locks = createLocks lockState.guids } model.lockRequests }
                    ! [ Postgres.query (RollbackError commandId) (Rollback commandId retryCount) connectionId "ROLLBACK" 1 ]
                , []
                )
            )
        ?!= (\_ -> ( model ! [], [ config.errorTagger ( FatalError, ( commandId, ("BUG -- Missing connectionId") ) ) ] ))


lockCmd : CommandId -> ConnectionId -> Int -> Int -> Cmd Msg
lockCmd commandId connectionId lock retryCount =
    let
        createLockStatement lock =
            "SELECT pg_try_advisory_xact_lock(" ++ (toString lock) ++ ");"
    in
        Postgres.query (LockEntitiesError commandId) (LockEntities commandId retryCount) connectionId (createLockStatement lock) 2


didLock : Config msg -> List String -> CommandId -> ( Bool, List msg )
didLock config results commandId =
    let
        fatal commandId error =
            config.errorTagger ( FatalError, ( commandId, error ) )
    in
        List.head results
            |?> (\result ->
                    JD.decodeString lockResponseDecoder result
                        |??> (\response -> ( response.pg_try_advisory_xact_lock, [] ))
                        ??= (\err -> ( False, [ fatal commandId ("SQL Lock Command results could not be decoded. Error:" +-+ err) ] ))
                )
            ?!= (\_ -> ( False, [ fatal commandId ("SQL Lock Command results list is empty") ] ))
