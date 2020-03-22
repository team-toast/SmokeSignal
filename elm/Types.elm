module Types exposing (..)

import Browser
import Browser.Navigation
import CommonTypes exposing (..)
import Contracts.SmokeSig as SSContract
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
    , testMode : Bool
    , txSentry : TxSentry Msg
    , eventSentry : EventSentry Msg
    , userBalance : Maybe TokenValue
    , userAllowance : Maybe TokenValue
    , messages : List Message
    , showMessageInput : Bool
    , composeUXModel : ComposeUXModel
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
    | ConnectToWeb3
    | UnlockDai
    | AllowanceFetched (Result Http.Error TokenValue)
    | BalanceFetched (Result Http.Error TokenValue)
    | ComposeMessage
    | MessageInputChanged String
    | DaiInputChanged String
    | Submit ValidatedInputs
    | Test String


type alias Message =
    { hash : String
    , block : Int
    , from : Address
    , burnAmount : TokenValue
    , message : String
    }


type alias ComposeUXModel =
    { message : String
    , daiInput : String
    }


updateMessage : String -> ComposeUXModel -> ComposeUXModel
updateMessage message m =
    { m
        | message = message
    }


updateDaiInput : String -> ComposeUXModel -> ComposeUXModel
updateDaiInput input m =
    { m
        | daiInput = input
    }


type alias ValidatedInputs =
    { message : String
    , burnAmount : TokenValue
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
                |> Result.map (ValidatedInputs composeModel.message)
            )


type alias AccountInfo =
    { address : Maybe Address
    , balance : Maybe TokenValue
    , isUnlocked : Maybe Bool
    }


makeAccountInfo : Model -> AccountInfo
makeAccountInfo model =
    AccountInfo
        (model.wallet
            |> Wallet.userInfo
            |> Maybe.map .address
        )
        model.userBalance
        (model.userAllowance
            |> Maybe.map
                (\allowance ->
                    TokenValue.compare allowance TokenValue.maxTokenValue == EQ
                )
        )
