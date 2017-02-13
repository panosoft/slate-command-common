module Slate.Command.Processor
    exposing
        ( Msg
        , Model
        , Config
        , CommandErrorTagger
        , CommandSuccessTagger
        , init
        , update
        , process
        )

{-|
    Command Processor for Entities.

@docs Msg , Model , Config, CommandErrorTagger, CommandSuccessTagger, init, update, process
-}

import Dict as Dict exposing (Dict)
import DebugF
import Maybe.Extra as MaybeE exposing (isNothing)
import StringUtils exposing ((+-+), (+++))
import Slate.Command.Helper as CommandHelper
import Slate.Command.Common.Validator exposing (..)
import Slate.Command.Common.Command exposing (..)
import ParentChildUpdate exposing (..)
import Utils.Ops exposing (..)
import Utils.Error exposing (..)
import Utils.Log exposing (..)
import Slate.Common.Entity exposing (..)
import Slate.Common.Db exposing (..)
import Services.Common.Taggers exposing (..)


commandHelperConfig : CommandHelper.Config Msg
commandHelperConfig =
    { retryMax = Nothing
    , delayNext = Nothing
    , lockRetries = Nothing
    , routeToMeTagger = CommandHelperModule
    , errorTagger = CommandHelperError
    , logTagger = CommandHelperLog
    , initCommandTagger = InitCommandSuccess
    , initCommandErrorTagger = InitCommandError
    , lockEntitiesTagger = LockEntitiesSuccess
    , lockEntitiesErrorTagger = LockEntitiesError
    , writeEventsTagger = WriteEventsSuccess
    , writeEventsErrorTagger = WriteEventsError
    , commitTagger = CommitSuccess
    , commitErrorTagger = CommitError
    , rollbackTagger = RollbackSuccess
    , rollbackErrorTagger = RollbackError
    , connectionLostTagger = ConnectionLost
    }


type alias CommandState msg =
    { dbConnectionInfo : DbConnectionInfo
    , lockEntityIds : List EntityReference
    , events : List String
    , maybeValidateTagger : Maybe (ValidateTagger Msg msg)
    }


type alias CommandStateDict msg =
    Dict CommandId (CommandState msg)


{-|
    Command Processor's Model
-}
type alias Model msg =
    { commandHelperModel : CommandHelper.Model
    , commandStates : CommandStateDict msg
    }


initModel : ( Model msg, List (Cmd Msg) )
initModel =
    let
        ( commandHelperModel, commandHelperCmds ) =
            CommandHelper.init commandHelperConfig
    in
        ( { commandHelperModel = commandHelperModel
          , commandStates = Dict.empty
          }
        , [ commandHelperCmds ]
        )


{-|
    Command Processor's Msg
-}
type Msg
    = Nop
    | InitCommandSuccess CommandId
    | InitCommandError ( CommandId, String )
    | LockEntitiesSuccess CommandId
    | LockEntitiesError ( CommandId, String )
    | WriteEventsSuccess ( CommandId, Int )
    | WriteEventsError ( CommandId, String )
    | CommitSuccess CommandId
    | CommitError ( CommandId, String )
    | RollbackSuccess String CommandId
    | RollbackError String ( CommandId, String )
    | ConnectionLost ( CommandId, String )
    | ValidationSuccess CommandId
    | ValidationError ( CommandId, String )
    | CommandHelperError ( ErrorType, ( CommandId, String ) )
    | CommandHelperLog ( LogLevel, ( CommandId, String ) )
    | CommandHelperModule CommandHelper.Msg


{-|
    Update.
-}
update : Config msg -> Msg -> Model msg -> ( ( Model msg, Cmd Msg ), List msg )
update config msg model =
    let
        logMsg commandId message =
            config.logTagger ( LogLevelInfo, ( commandId, message ) )

        nonFatal commandId error =
            config.errorTagger ( NonFatalError, ( commandId, error ) )

        fatal commandId error =
            config.errorTagger ( FatalError, ( commandId, error ) )

        getCommandState commandId =
            Dict.get commandId model.commandStates
                ?!= (\_ -> Debug.crash "BUG -- Command Id not in dictionary")

        getLockEntityIds commandId =
            .lockEntityIds <| getCommandState commandId

        events commandId =
            .events <| getCommandState commandId

        commandFailedMsg commandId error =
            config.commandErrorTagger ( commandId, error )

        removeCommand model commandId =
            { model | commandStates = Dict.remove commandId model.commandStates }

        rollbackLessFailure model commandId maybeRollbackError originalError =
            let
                msgs =
                    maybeRollbackError
                        |?> (\rollbackError -> [ nonFatal commandId rollbackError ])
                        ?= []
            in
                ( removeCommand model commandId ! [ Cmd.none ], List.append msgs [ commandFailedMsg commandId originalError ] )

        helperFailed model commandId originalError =
            let
                l =
                    Debug.log (toString msg) +-+ " Failed with Error (Command Id:  " +-+ commandId +-+ "Error:" +-+ originalError
            in
                CommandHelper.rollback commandHelperConfig model.commandHelperModel commandId originalError
                    |??> (\( commandHelperModel, cmd ) -> ( { model | commandHelperModel = commandHelperModel } ! [ cmd ], [] ))
                    ??= (\rollbackError -> rollbackLessFailure model commandId (Just rollbackError) originalError)

        helperResults model commandId result =
            let
                ( ( newModel, cmd ), msgs ) =
                    result
                        |??> (\( commandHelperModel, cmd ) -> ( { model | commandHelperModel = commandHelperModel } ! [ cmd ], [] ))
                        ??= helperFailed model commandId
            in
                ( newModel ! [ cmd ], msgs )

        updateCommandHelper =
            ParentChildUpdate.updateChildParent (CommandHelper.update commandHelperConfig) (update config) .commandHelperModel CommandHelperModule (\model commandHelperModel -> { model | commandHelperModel = commandHelperModel })
    in
        case msg of
            Nop ->
                ( model ! [], [] )

            InitCommandSuccess commandId ->
                let
                    l =
                        Debug.log "InitCommand Complete" ("Command Id:  " +-+ commandId)
                in
                    CommandHelper.lockEntities commandHelperConfig model.commandHelperModel commandId (getLockEntityIds commandId)
                        |> helperResults model commandId

            InitCommandError ( commandId, error ) ->
                rollbackLessFailure model commandId Nothing error

            LockEntitiesSuccess commandId ->
                let
                    l =
                        Debug.log "LockEntities Complete" ("Command Id:  " +-+ commandId)

                    crashInfo =
                        "(CommandId:" +-+ commandId +-+ ", CommandState:" +-+ commandState +-+ ")"

                    checkLocks =
                        let
                            lockEntityIds =
                                getLockEntityIds commandId

                            validationExists =
                                (not << isNothing) commandState.maybeValidateTagger

                            locksExist =
                                lockEntityIds /= []

                            locksWithValidationOnly =
                                case validationExists of
                                    True ->
                                        (not locksExist) ?! ( \_ -> Debug.crash <| "Validation cannot be performed without any entityId locks" +-+ crashInfo, identity )

                                    False ->
                                        (locksExist) ?! ( \_ -> Debug.crash <| "EntityId locks cannot exist without validation" +-+ crashInfo, identity )

                            noEmptyLockIds =
                                (List.member "" lockEntityIds) ?! ( \_ -> Debug.crash <| "EntityId locks cannot be an empty string" +-+ crashInfo, identity )
                        in
                            ()

                    commandState =
                        getCommandState commandId
                in
                    commandState.maybeValidateTagger
                        |?> (\validateTagger ->
                                let
                                    doCheck =
                                        checkLocks
                                in
                                    ( model ! []
                                    , [ validateTagger ValidationError ValidationSuccess commandId commandState.dbConnectionInfo ]
                                    )
                            )
                        ?= update config (ValidationSuccess commandId) model

            LockEntitiesError ( commandId, error ) ->
                helperFailed model commandId error

            WriteEventsSuccess ( commandId, eventRows ) ->
                CommandHelper.commit commandHelperConfig model.commandHelperModel commandId
                    |> helperResults model commandId

            WriteEventsError ( commandId, error ) ->
                helperFailed model commandId error

            CommitSuccess commandId ->
                ( removeCommand model commandId ! [], [ config.commandSuccessTagger commandId ] )

            CommitError ( commandId, error ) ->
                helperFailed model commandId error

            RollbackSuccess originalError commandId ->
                ( removeCommand model commandId ! [], [ commandFailedMsg commandId originalError ] )

            RollbackError originalError ( commandId, error ) ->
                ( removeCommand model commandId ! [], [ nonFatal commandId error, commandFailedMsg commandId originalError ] )

            ConnectionLost ( commandId, error ) ->
                rollbackLessFailure model commandId Nothing error

            ValidationSuccess commandId ->
                CommandHelper.writeEvents commandHelperConfig model.commandHelperModel commandId (events commandId)
                    |> helperResults model commandId

            ValidationError ( commandId, error ) ->
                helperFailed model commandId error

            CommandHelperError ( errorType, ( commandId, error ) ) ->
                let
                    l =
                        case errorType of
                            NonFatalError ->
                                DebugF.log "CommandHelperError" error

                            _ ->
                                Debug.crash <| toString error
                in
                    ( model ! [], [ config.errorTagger ( errorType, ( commandId, error ) ) ] )

            CommandHelperLog ( logLevel, ( commandId, details ) ) ->
                let
                    l =
                        DebugF.log "CommandHelperLog" (toString logLevel ++ ":" +-+ details)
                in
                    ( model ! [], [ config.logTagger ( logLevel, ( commandId, details ) ) ] )

            CommandHelperModule msg ->
                updateCommandHelper msg model



-- API


{-|
    Parent's Tagger that will result in calling this modules update function.
-}
type alias RouteToMeTagger msg =
    Msg -> msg


{-|
    Tagger for command errors.
-}
type alias CommandErrorTagger msg =
    ( CommandId, String ) -> msg


{-|
    Tagger for command success.
-}
type alias CommandSuccessTagger msg =
    CommandId -> msg


{-|
    Command Processor's Config
-}
type alias Config msg =
    { routeToMeTagger : RouteToMeTagger msg
    , errorTagger : ErrorTagger ( CommandId, String ) msg
    , logTagger : LogTagger ( CommandId, String ) msg
    , commandErrorTagger : CommandErrorTagger msg
    , commandSuccessTagger : CommandSuccessTagger msg
    }


{-|
    Initialize command helper
-}
init : Config msg -> ( Model msg, Cmd msg )
init config =
    let
        ( model, cmds ) =
            initModel
    in
        model ! (List.map (Cmd.map config.routeToMeTagger) cmds)


{-|
    Process command.
-}
process : Config msg -> DbConnectionInfo -> Maybe (ValidateTagger Msg msg) -> List String -> List EntityReference -> Model msg -> ( Model msg, Cmd msg, CommandId )
process config dbConnectionInfo maybeValidateTagger lockEntityIds events model =
    let
        ( commandHelperModel, cmd, commandId ) =
            CommandHelper.initCommand commandHelperConfig dbConnectionInfo model.commandHelperModel

        commandStates =
            Dict.insert commandId
                { dbConnectionInfo = dbConnectionInfo
                , lockEntityIds = lockEntityIds
                , events = events
                , maybeValidateTagger = maybeValidateTagger
                }
                model.commandStates
    in
        ( { model | commandHelperModel = commandHelperModel, commandStates = commandStates }, Cmd.map config.routeToMeTagger cmd, commandId )
