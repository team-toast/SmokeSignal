module ComposeUX.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Eth.Types exposing (Address, TxHash)
import Http
import Post
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


type alias Model =
    { message : String
    , daiInput : String
    , donateChecked : Bool
    , miningUnlockTx : Maybe TxHash
    , wallet : Wallet
    }


type Msg
    = MsgUp MsgUp
    | MessageInputChanged String
    | DaiInputChanged String
    | DonationCheckboxSet Bool


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


type ComposeContext
    = ComposingReply Post.Id String
    | ComposingForTopic String


composeContextToMetadata : ComposeContext -> Post.Metadata
composeContextToMetadata context =
    case context of
        ComposingReply replyTo topic ->
            Post.versionedMetadata
                (Just replyTo)
                (Just topic)

        ComposingForTopic topic ->
            Post.versionedMetadata
                Nothing
                (Just topic)


composeContextReplyTo : ComposeContext -> Maybe Post.Id
composeContextReplyTo context =
    case context of
        ComposingReply replyTo _ ->
            Just replyTo

        _ ->
            Nothing


composeContextTopic context =
    case context of
        ComposingForTopic topic ->
            Just topic

        _ ->
            Nothing

updateMessage : String -> Model -> Model
updateMessage message m =
    { m | message = message }


updateDaiInput : String -> Model -> Model
updateDaiInput input m =
    { m | daiInput = input }


updateDonateChecked : Bool -> Model -> Model
updateDonateChecked flag m =
    { m | donateChecked = flag }


updateMiningUnlockTx : Maybe TxHash -> Model -> Model
updateMiningUnlockTx maybeTxHash m =
    { m | miningUnlockTx = maybeTxHash }


type alias CheckedMaybeValidInputs =
    { message : Maybe String
    , burnAndDonateAmount : Maybe (Result String ( TokenValue, TokenValue ))
    }


validateInputs : Model -> CheckedMaybeValidInputs
validateInputs composeModel =
    { message =
        if composeModel.message == "" then
            Nothing

        else
            Just composeModel.message
    , burnAndDonateAmount =
        validateBurnAmount composeModel.daiInput
            |> Maybe.map
                (Result.map
                    (\burnAmount ->
                        ( burnAmount
                        , if composeModel.donateChecked then
                            TokenValue.div burnAmount 100

                          else
                            TokenValue.zero
                        )
                    )
                )
    }


validateBurnAmount : String -> Maybe (Result String TokenValue)
validateBurnAmount input =
    if input == "" then
        Nothing

    else
        Just
            (TokenValue.fromString input
                |> Result.fromMaybe "Invalid burn amount"
                |> Result.andThen
                    (\tv ->
                        if TokenValue.compare tv TokenValue.zero == GT then
                            Ok tv

                        else
                            Err "Minimum amount is 0.000000000000000001 DAI"
                    )
            )
