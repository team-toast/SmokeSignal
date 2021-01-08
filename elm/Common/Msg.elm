module Common.Msg exposing (..)

import Common.Types exposing (..)
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, TxHash)
import Post
import Routing exposing (Route)
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Wallet exposing (Wallet)


type MsgUp
    = StartInlineCompose Post.Context
    | ExitCompose
    | GotoRoute Route
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | AddUserNotice UN.UserNotice
    | UnlockDai
    | SubmitPost Post.Draft
    | SubmitTip Post.Id TokenValue
    | SubmitBurn Post.Id TokenValue
    | DonationCheckboxSet Bool
    | NoOp


type MsgDown
    = UpdateWallet Wallet
