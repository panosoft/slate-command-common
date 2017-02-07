module Slate.Command.Common.Validator
    exposing
        ( ValidateTagger
        , ValidateSuccessTagger
        , ValidateErrorTagger
        )

{-|
    Common Validator definitions.

@docs ValidateTagger, ValidateSuccessTagger, ValidateErrorTagger
-}

import Slate.Command.Common.Command exposing (..)
import Slate.Common.Event exposing (..)
import Slate.Common.Db exposing (..)


{-|
    Tagger to create a message to signal to the Command Processor that a validation succeeded.
-}
type alias ValidateSuccessTagger commandProcessorMsg =
    CommandId -> commandProcessorMsg


{-|
    Tagger to create a message to signal to the Command Processor that a validation failed.
-}
type alias ValidateErrorTagger msg =
    ( CommandId, String ) -> msg


{-|
    Tagger to create Command Processor's Parent Msg
-}
type alias ValidateTagger commandProcessorMsg msg =
    ValidateErrorTagger commandProcessorMsg
    -> ValidateSuccessTagger commandProcessorMsg
    -> CommandId
    -> DbConnectionInfo
    -> msg
