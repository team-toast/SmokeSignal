module Common.Types exposing (..)

import Dict
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, Hex, TxHash)
import Json.Decode
import Json.Encode
import Post
import TokenValue exposing (TokenValue)


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    , balance : Maybe TokenValue
    , daiUnlocked : Maybe Bool
    }


withBalance : TokenValue -> UserInfo -> UserInfo
withBalance balance userInfo =
    { userInfo
        | balance = Just balance
    }


withIsUnlocked : Bool -> UserInfo -> UserInfo
withIsUnlocked unlocked userInfo =
    { userInfo
        | daiUnlocked = Just <| unlocked
    }


type PhaceIconId
    = PhaceForMinedMessage Post.Id
    | PhaceForUserMiningMessage TxHash
    | User


type alias TrackedTx =
    { txInfo : TxInfo
    , status : TxStatus
    }


type TxInfo
    = PostTx Post.Draft
    | UnlockTx


type TxStatus
    = Mining
    | Failed String
    | Mined


txInfoToNameStr txInfo =
    case txInfo of
        UnlockTx ->
            "Unlock DAI"

        PostTx _ ->
            "Post Submit"
