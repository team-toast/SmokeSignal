module Types exposing (..)

import Browser
import Browser.Navigation
import CommonTypes exposing (..)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Time
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
    , txSentry : Maybe (TxSentry Msg)
    , eventSentry : EventSentry Msg
    }


type Msg
    = NoOp
    | ClickHappened
    | Tick Time.Posix
    | ConnectToWeb3
    | WalletStatus (Result String WalletSentry)
    | TxSentryMsg TxSentry.Msg
    | EventSentryMsg EventSentry.Msg
    | MessageLogReceived Eth.Types.Log
    | Test String
