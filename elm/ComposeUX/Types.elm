module ComposeUX.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Element exposing (Element)
import Eth.Types exposing (Address, TxHash)
import Http
import Post
import Time
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


type alias Model =
    { now : Time.Posix
    , context : Post.Context
    , message : String
    , daiInput : String
    , donateChecked : Bool
    , wallet : Wallet
    , showPreviewOnMobile : Bool
    , lastInputChangedTime : Time.Posix
    , renderNeeded : Bool
    , renderedPreview : Maybe (Element Never)
    }


type Msg
    = MsgUp MsgUp
    | MessageInputChanged String
    | DaiInputChanged String
    | DonationCheckboxSet Bool
    | MobilePreviewToggle
    | Tick Time.Posix


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


updateContext : Post.Context -> Model -> Model
updateContext context m =
    { m | context = context }


updateMessage : String -> Model -> Model
updateMessage message m =
    { m | message = message }


updateDaiInput : String -> Model -> Model
updateDaiInput input m =
    { m | daiInput = input }


updateDonateChecked : Bool -> Model -> Model
updateDonateChecked flag m =
    { m | donateChecked = flag }


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


subscriptions : Sub Msg
subscriptions =
    Time.every 200 Tick
