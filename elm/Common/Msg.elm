module Common.Msg exposing (..)

import Common.Types exposing (..)
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, TxHash)
import Post
import TokenValue exposing (TokenValue)
import UserNotice as UN


type MsgUp
    = StartInlineCompose Context
    | ExitCompose
    | GotoRoute Route
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | AddUserNotice UN.UserNotice
    | UnlockDai
    | SubmitPost Draft
    | SubmitTip Id TokenValue
    | SubmitBurn Id TokenValue
    | DonationCheckboxSet Bool
    | NoOp


type MsgDown
    = UpdateWallet Wallet
