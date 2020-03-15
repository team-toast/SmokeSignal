module Types exposing (..)

import Browser
import Browser.Navigation
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Time
import Url
import Wallet
import CommonTypes exposing (..)

type alias Flags =
    { networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
    }


type alias Model =
    { key : Browser.Navigation.Key
    , testMode : Bool
    , wallet : Wallet.State
    , now : Time.Posix
    , txSentry : Maybe (TxSentry Msg)
    , dProfile : DisplayProfile
    , maybeReferrer : Maybe Address
    }


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ClickHappened
    | Tick Time.Posix
    | ConnectToWeb3
    | WalletStatus WalletSentry
    | TxSentryMsg TxSentry.Msg
    | Resize Int Int
    | Test String
