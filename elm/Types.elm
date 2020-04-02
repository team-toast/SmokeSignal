module Types exposing (..)

import Browser
import Browser.Navigation
import CommonTypes exposing (..)
import Contracts.SmokeSignal as SSContract
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Http
import Time
import TokenValue exposing (TokenValue)
import Url
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
    , messages : List Message
    , showingAddress : Maybe Address
    , showComposeUX : Bool
    , composeUXModel : ComposeUXModel
    , blockTimes : Dict Int Time.Posix
    }


type Msg
    = NoOp
    | ClickHappened
    | Tick Time.Posix
    | EveryFewSeconds
    | WalletStatus (Result String WalletSentry)
    | TxSentryMsg TxSentry.Msg
    | EventSentryMsg EventSentry.Msg
    | MessageLogReceived Eth.Types.Log
    | ShowAddress Address
    | HideAddress
    | ConnectToWeb3
    | UnlockDai
    | AllowanceFetched Address (Result Http.Error TokenValue)
    | BalanceFetched Address (Result Http.Error TokenValue)
    | ShowComposeUX Bool
    | MessageInputChanged String
    | DaiInputChanged String
    | DonationCheckboxSet Bool
    | Submit ValidatedInputs
    | BlockTimeFetched Int (Result Http.Error Time.Posix)
    | Test String


type alias Message =
    { hash : Eth.Types.Hex
    , block : Int
    , from : Address
    , burnAmount : TokenValue
    , message : String
    }


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


type alias ValidatedInputs =
    { message : String
    , burnAmount : TokenValue
    , donateAmount : TokenValue
    }


validateInputs : ComposeUXModel -> Maybe (Result String ValidatedInputs)
validateInputs composeModel =
    if composeModel.message == "" || composeModel.daiInput == "" then
        Nothing

    else
        Just
            (TokenValue.fromString composeModel.daiInput
                |> Result.fromMaybe "Invalid burn amount"
                |> Result.andThen
                    (\tv ->
                        if TokenValue.compare tv TokenValue.zero == GT then
                            Ok tv

                        else
                            Err "Must be greater than 0"
                    )
                |> Result.map
                    (\burnAmount ->
                        ValidatedInputs
                            composeModel.message
                            burnAmount
                            (if composeModel.donateChecked then
                                TokenValue.div burnAmount 100

                             else
                                TokenValue.zero
                            )
                    )
            )



