module Types exposing (..)

import Browser
import Browser.Navigation
import Common.Msg exposing (MsgUp(..))
import Common.Types exposing (Accounting, Draft, Id, PhaceIconId, Published, PublishedPostsDict, ReplyIds, Route, TrackedTx, TxInfo, ViewContext, Wallet)
import ComposeUX.Types as ComposeUX
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet exposing (WalletSentry)
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Helpers.Element as EH
import Home.Types as Home
import Http
import List.Extra
import Misc exposing (viewContextToMaybeTitlePart)
import Post
import PostUX.Types as PostUX
import Routing
import Time
import TokenValue exposing (TokenValue)
import TopicUX.Types as TopicUX
import Url exposing (Url)
import UserNotice exposing (UserNotice)


type alias Flags =
    { basePath : String
    , networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
    , cookieConsent : Bool
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , basePath : String
    , route : Route
    , wallet : Wallet
    , now : Time.Posix
    , dProfile : EH.DisplayProfile
    , txSentry : TxSentry Msg
    , eventSentry : EventSentry Msg
    , publishedPosts : PublishedPostsDict

    --, postUX : Maybe ( PostUXId, PostUX.Model )
    , replies : List ReplyIds
    , mode : Mode
    , showHalfComposeUX : Bool

    --, composeUXModel : Maybe ComposeUX.Model
    , blockTimes : Dict Int Time.Posix
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : List TrackedTx -- Can't use TxHash as a key; Elm is silly with what is and is not comparable
    , showExpandedTrackedTxs : Bool
    , draftModal : Maybe Draft
    , demoPhaceSrc : String
    , donateChecked : Bool
    , cookieConsentGranted : Bool
    , maybeSeoDescription : Maybe String
    , searchInput : String

    --, topicUXModel : Maybe TopicUX.Model
    }


type PostUXId
    = PublishedPost Id
    | DraftPreview


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Tick Time.Posix
    | EveryFewSeconds
    | ChangeDemoPhaceSrc
    | NewDemoSrc String
      -- | MutateDemoSrcWith MutateInfo
    | Resize Int Int
    | WalletStatus (Result String WalletSentry)
    | TxSentryMsg TxSentry.Msg
    | EventSentryMsg EventSentry.Msg
    | PostLogReceived Eth.Types.Log
    | PostAccountingFetched Id (Result Http.Error Accounting)
    | ShowExpandedTrackedTxs Bool
    | CheckTrackedTxsStatus
    | TrackedTxStatusResult (Result Http.Error TxReceipt)
    | TxSigned TxInfo (Result String TxHash)
    | ViewDraft (Maybe Draft)
    | BlockTimeFetched Int (Result Http.Error Time.Posix)
    | RestoreDraft Draft
    | DismissNotice Int
    | ClickHappened
    | PostUXMsg PostUXId PostUX.Msg
    | ComposeUXMsg ComposeUX.Msg
    | TopicUXMsg TopicUX.Msg
    | HomeMsg Home.Msg
    | AllowanceFetched Address (Result Http.Error TokenValue)
    | BalanceFetched Address (Result Http.Error TokenValue)
    | CookieConsentGranted
    | MsgUp MsgUp


type Mode
    = BlankMode
    | ModeHome Home.Model
    | ModeCompose
    | ViewContext ViewContext


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
    case model.mode of
        BlankMode ->
            defaultMain

        ModeHome homeModel ->
            defaultMain

        ModeCompose ->
            "Compose | SmokeSignal"

        ViewContext context ->
            viewContextToMaybeTitlePart model.publishedPosts context
                |> Maybe.map (\contextTitle -> contextTitle ++ " | SmokeSignal")
                |> Maybe.withDefault defaultMain
