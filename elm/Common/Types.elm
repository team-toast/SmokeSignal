module Common.Types exposing (..)

import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, Hex, TxHash)
import Json.Decode
import Json.Encode
import Post exposing (Post)
import TokenValue exposing (TokenValue)


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    , balance : Maybe TokenValue
    , unlockStatus : UnlockStatus
    }


type UnlockStatus
    = NotConnected
    | Checking
    | Locked
    | Unlocking
    | Unlocked


type ViewContext
    = Post Post.Id
    | Topic String


withBalance : TokenValue -> UserInfo -> UserInfo
withBalance balance userInfo =
    { userInfo
        | balance = Just balance
    }


withUnlockStatus : UnlockStatus -> UserInfo -> UserInfo
withUnlockStatus unlockStatus userInfo =
    { userInfo
        | unlockStatus = unlockStatus
    }


type alias PublishedPostsDict =
    Dict Int (List Post.Published)


getPublishedPostFromId : PublishedPostsDict -> Post.Id -> Maybe Post.Published
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


getPublishedPostFromTxHash : PublishedPostsDict -> TxHash -> Maybe Post.Published
getPublishedPostFromTxHash publishedPosts txHash =
    publishedPosts
        |> Dict.values
        |> List.concat
        |> List.filter
            (\publishedPost ->
                publishedPost.txHash == txHash
            )
        |> List.head


viewContextToMaybeTitlePart : PublishedPostsDict -> ViewContext -> Maybe String
viewContextToMaybeTitlePart posts context =
    case context of
        Post postId ->
            getPublishedPostFromId posts postId
                |> Maybe.andThen (.core >> .content >> .title)

        Topic topic ->
            Just <| "#" ++ topic


viewContextToMaybeDescription : PublishedPostsDict -> ViewContext -> Maybe String
viewContextToMaybeDescription posts context =
    case context of
        Post postId ->
            getPublishedPostFromId posts postId
                |> Maybe.andThen (.core >> .content >> .desc)

        Topic topic ->
            Just <| "Discussions related to #" ++ topic ++ " on SmokeSignal"


postContextToViewContext : Post.Context -> ViewContext
postContextToViewContext postContext =
    case postContext of
        Post.Reply id ->
            Post id

        Post.TopLevel topicStr ->
            Topic topicStr


viewContextToPostContext : ViewContext -> Post.Context
viewContextToPostContext viewContext =
    case viewContext of
        Post id ->
            Post.Reply id

        Topic topicStr ->
            Post.TopLevel topicStr


defaultSeoDescription : String
defaultSeoDescription =
    "SmokeSignal - Uncensorable, Global, Immutable chat. Burn crypto to cement your writing on the blockchain. Grant your ideas immortality."


updatePublishedPost : Post.Id -> (Post.Published -> Post.Published) -> PublishedPostsDict -> PublishedPostsDict
updatePublishedPost postId updateFunc posts =
    posts
        |> Dict.update postId.block
            (Maybe.map <|
                List.map
                    (\thisPost ->
                        if thisPost.id == postId then
                            updateFunc thisPost

                        else
                            thisPost
                    )
            )


type alias Reply =
    { from : Post.Id
    , to : Post.Id
    }


type PhaceIconId
    = PhaceForPublishedPost Post.Id
    | PhaceForDraft
    | PhaceForPreview
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
    | TipTx Post.Id TokenValue
    | BurnTx Post.Id TokenValue


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

        TipTx postId amount ->
            "Tip"

        BurnTx postId amount ->
            "Burn"


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


type alias GTagData =
    { event : String
    , category : String
    , label : String
    , value : Int
    }
