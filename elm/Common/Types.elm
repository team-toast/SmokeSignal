module Common.Types exposing (..)

import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, Hex, TxHash)
import Json.Decode
import Json.Encode
import Post exposing (Post, PublishedPost)
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


type alias PublishedPostsDict =
    Dict Int (List PublishedPost)


getPublishedPostFromId : PublishedPostsDict -> Post.Id -> Maybe PublishedPost
getPublishedPostFromId publishedPosts postId =
    publishedPosts
        |> Dict.get postId.block
        |> Maybe.map
            (List.filter
                (\post ->
                    post.id.messageHash == postId.messageHash
                )
            )
        |> Maybe.andThen List.head


getPublishedPostFromTxHash : PublishedPostsDict -> TxHash -> Maybe PublishedPost
getPublishedPostFromTxHash publishedPosts txHash =
    publishedPosts
        |> Dict.values
        |> List.concat
        |> List.filter
            (\publishedPost ->
                publishedPost.txHash == txHash
            )
        |> List.head


type alias Reply =
    { from : Post.Id
    , to : Post.Id
    }


type PhaceIconId
    = PhaceForPublishedPost Post.Id
    | PhaceForDraft
    | UserPhace
    | MorphingPhace


type alias TrackedTx =
    { txHash : TxHash
    , txInfo : TxInfo
    , status : TxStatus
    }


type TxInfo
    = PostTx Post.Draft
    | UnlockTx


type TxStatus
    = Mining
    | Failed FailReason
    | Mined (Maybe Post.Id)


type FailReason
    = MinedButExecutionFailed


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
