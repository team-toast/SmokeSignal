module Types exposing (..)

import Browser
import Browser.Navigation
import CommonTypes exposing (..)
import Contracts.SmokeSignal as SSContract
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address, Hex)
import Http
import Time
import TokenValue exposing (TokenValue)
import Url
import UserNotice as UN exposing (UserNotice)
import Wallet


type alias Flags =
    { networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
    }


type alias Model =
    { wallet : Wallet.State
    , now : Time.Posix
    , txSentry : TxSentry Msg
    , eventSentry : EventSentry Msg
    , messages : Dict String Message -- Can't use Hex type, as Elm is silly with what is and is not comparable
    , showingAddress : Maybe Address
    , showComposeUX : Bool
    , composeUXModel : ComposeUXModel
    , blockTimes : Dict Int Time.Posix
    , showAddress : Maybe PhaceId
    , userNotices : List (UserNotice Msg)
    }


type Msg
    = NoOp
    | Tick Time.Posix
    | EveryFewSeconds
    | WalletStatus (Result String WalletSentry)
    | TxSentryMsg TxSentry.Msg
    | EventSentryMsg EventSentry.Msg
    | MessageLogReceived Eth.Types.Log
    | ShowAddress PhaceId
    | HideAddress
    | ConnectToWeb3
    | UnlockDai
    | AllowanceFetched Address (Result Http.Error TokenValue)
    | BalanceFetched Address (Result Http.Error TokenValue)
    | ShowComposeUX Bool
    | MessageInputChanged String
    | DaiInputChanged String
    | DonationCheckboxSet Bool
    | Submit ValidInputs
    | BlockTimeFetched Int (Result Http.Error Time.Posix)
    | DismissNotice Int


type alias Message =
    { block : Int
    , author : Address
    , burnAmount : TokenValue
    , message : String
    }


type PhaceId
    = MessageAuthor Hex
    | User


type alias ComposeUXModel =
    { message : String
    , daiInput : String
    , donateChecked : Bool
    }


updateMessage : String -> ComposeUXModel -> ComposeUXModel
updateMessage message m =
    { m | message = message }


updateDaiInput : String -> ComposeUXModel -> ComposeUXModel
updateDaiInput input m =
    { m | daiInput = input }


updateDonateChecked : Bool -> ComposeUXModel -> ComposeUXModel
updateDonateChecked flag m =
    { m | donateChecked = flag }


type alias CheckedMaybeValidInputs =
    { message : Maybe String
    , burnAndDonateAmount : Maybe (Result String ( TokenValue, TokenValue ))
    }


type alias ValidInputs =
    { message : String
    , burnAmount : TokenValue
    , donateAmount : TokenValue
    }


validateInputs : ComposeUXModel -> CheckedMaybeValidInputs
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
                            Err "Must be greater than 0"
                    )
            )
