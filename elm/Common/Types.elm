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


type ComposeContext
    = ComposingReply Post.Id
    | ComposingForTopic String


type alias ViewContext =
    { showReplyTo : Bool
    , showTopic : Bool
    }


type alias Reply =
    { from : Post.Id
    , to : Post.Id
    }


type PhaceIconId
    = PhaceForPostAuthor Post.Id
    | UserPhace
    | MorphingPhace


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


type WalletUXPhaceInfo
    = UserPhaceInfo ( UserInfo, Bool )
    | DemoPhaceInfo String


makeWalletUXPhaceInfo : Maybe UserInfo -> Maybe PhaceIconId -> String -> WalletUXPhaceInfo
makeWalletUXPhaceInfo maybeUserInfo maybeShowAddressId demoPhaceSrc =
    case maybeUserInfo of
        Just userInfo ->
            UserPhaceInfo ( userInfo, maybeShowAddressId == Just UserPhace )

        Nothing ->
            DemoPhaceInfo demoPhaceSrc
