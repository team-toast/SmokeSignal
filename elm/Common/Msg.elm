module Common.Msg exposing (..)

import Eth.Types exposing (Address, TxHash)
import Post
import Common.Types exposing (..)
import Wallet exposing (Wallet)
import Common.Types exposing (..)
import Routing exposing (Route)
import UserNotice as UN
import Eth.Sentry.Tx as TxSentry

type MsgUp
    = ShowHalfComposeUX Bool
    | GotoRoute Route
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | AddUserNotice UN.UserNotice
    | UnlockDai
    | SubmitPost Post.Draft

type MsgDown
    = UpdateWallet Wallet
    | PostSigned Post.Draft