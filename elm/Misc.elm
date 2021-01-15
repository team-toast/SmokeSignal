module Misc exposing (..)

import Browser.Navigation
import Dict
import Eth.Sentry.Event exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, Hex, TxHash)
import Helpers.Element
import List.Extra
import Maybe.Extra exposing (unwrap)
import Ports
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)


emptyModel : Browser.Navigation.Key -> Model
emptyModel key =
    { navKey = key
    , basePath = ""
    , route = Types.Home
    , wallet = Types.NoneDetected
    , now = Time.millisToPosix 0
    , dProfile = Helpers.Element.Desktop
    , txSentry =
        TxSentry.init
            ( Ports.txOut, Ports.txIn )
            TxSentryMsg
            ""
    , eventSentry = Eth.Sentry.Event.init (always Types.ClickHappened) "" |> Tuple.first
    , publishedPosts = Dict.empty

    --, postUX = Nothing
    , replies = []
    , view = ViewHome
    , showHalfComposeUX = False

    --, composeUXModel = Nothing -- TODO ComposeUX.init now (TopLevel Post.defaultTopic)
    , blockTimes = Dict.empty
    , showAddressId = Nothing
    , userNotices = []
    , trackedTxs = []
    , showExpandedTrackedTxs = False
    , draftModal = Nothing
    , demoPhaceSrc = ""
    , donateChecked = True
    , cookieConsentGranted = False
    , maybeSeoDescription = Nothing
    , searchInput = ""

    --, topicUXModel = Nothing
    }


filterPosts : (Published -> Bool) -> PublishedPostsDict -> PublishedPostsDict
filterPosts filterFunc =
    Dict.map
        (always <| List.filter filterFunc)
        >> Dict.filter
            (\_ publishedPosts ->
                not <| publishedPosts == []
            )


updateTrackedTxByTxInfo : TxInfo -> (TrackedTx -> TrackedTx) -> Model -> Model
updateTrackedTxByTxInfo txInfo =
    updateTrackedTxIf
        (.txInfo >> (==) txInfo)


updateTrackedTxByTxHash : TxHash -> (TrackedTx -> TrackedTx) -> Model -> Model
updateTrackedTxByTxHash txHash =
    updateTrackedTxIf
        (.txHash >> (==) txHash)


updateTrackedTxIf : (TrackedTx -> Bool) -> (TrackedTx -> TrackedTx) -> Model -> Model
updateTrackedTxIf test update model =
    { model
        | trackedTxs =
            model.trackedTxs
                |> List.Extra.updateIf
                    test
                    update
    }


getTitle : Model -> String
getTitle model =
    let
        defaultMain =
            "SmokeSignal | Uncensorable - Immutable - Unkillable | Real Free Speech - Cemented on the Blockchain"
    in
    case model.view of
        ViewHome ->
            defaultMain

        ViewCompose ->
            "Compose | SmokeSignal"

        ViewPost postId ->
            getPublishedPostFromId model.publishedPosts postId
                |> Maybe.andThen (.core >> .content >> .title)
                |> unwrap defaultMain (\contextTitle -> contextTitle ++ " | SmokeSignal")

        ViewTopic topic ->
            "#" ++ topic ++ " | SmokeSignal"


withBalance :
    TokenValue
    -> UserInfo
    -> UserInfo
withBalance balance userInfo =
    { userInfo
        | balance = Just balance
    }


withUnlockStatus :
    UnlockStatus
    -> UserInfo
    -> UserInfo
withUnlockStatus unlockStatus userInfo =
    { userInfo
        | unlockStatus = unlockStatus
    }


getPublishedPostFromId :
    PublishedPostsDict
    -> Id
    -> Maybe Published
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


getPublishedPostFromTxHash :
    PublishedPostsDict
    -> TxHash
    -> Maybe Published
getPublishedPostFromTxHash publishedPosts txHash =
    publishedPosts
        |> Dict.values
        |> List.concat
        |> List.filter
            (\publishedPost ->
                publishedPost.txHash == txHash
            )
        |> List.head



--viewContextToMaybeDescription :
--PublishedPostsDict
---> ViewContext
---> Maybe String
--viewContextToMaybeDescription posts context =
--case context of
--ViewPost postId ->
--getPublishedPostFromId posts postId
--|> Maybe.andThen (.core >> .content >> .desc)
--Topic topic ->
--Just <| "Discussions related to #" ++ topic ++ " on SmokeSignal"
--postContextToViewContext :
--Context
---> ViewContext
--postContextToViewContext postContext =
--case postContext of
--Reply id ->
--ViewPost id
--TopLevel topicStr ->
--Topic topicStr
--viewContextToPostContext :
--ViewContext
---> Context
--viewContextToPostContext viewContext =
--case viewContext of
--ViewPost id ->
--Reply id
--Topic topicStr ->
--TopLevel topicStr


defaultSeoDescription : String
defaultSeoDescription =
    "SmokeSignal - Uncensorable, Global, Immutable chat. Burn crypto to cement your writing on the blockchain. Grant your ideas immortality."


updatePublishedPost :
    Id
    -> (Published -> Published)
    -> PublishedPostsDict
    -> PublishedPostsDict
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
