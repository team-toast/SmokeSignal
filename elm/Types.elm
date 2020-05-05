module Types exposing (..)

import Array exposing (Array)
import Browser
import Browser.Navigation
import Common.Msg exposing (..)
import Common.Types as Common exposing (..)
import ComposeUX.Types as ComposeUX
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator exposing (MutateInfo)
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address, Hex, Tx, TxHash, TxReceipt)
import Eth.Utils
import Helpers.Element as EH
import Home.Types as Home
import Http
import List.Extra
import Post exposing (Post)
import PostUX.Types as PostUX
import Routing exposing (Route)
import Time
import TokenValue exposing (TokenValue)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import Wallet exposing (Wallet)


type alias Flags =
    { basePath : String
    , networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
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
    , postUX : Maybe ( PostUXId, PostUX.Model )
    , replies : List Reply
    , mode : Mode
    , showHalfComposeUX : Bool
    , composeUXModel : ComposeUX.Model
    , blockTimes : Dict Int Time.Posix
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : List TrackedTx -- Can't use TxHash as a key; Elm is silly with what is and is not comparable
    , showExpandedTrackedTxs : Bool
    , draftModal : Maybe Post.Draft
    , demoPhaceSrc : String
    }


type PostUXId
    = PublishedPost Post.Id
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
    | PostAccountingFetched Post.Id (Result Http.Error Post.Accounting)
    | ShowExpandedTrackedTxs Bool
    | CheckTrackedTxsStatus
    | TrackedTxStatusResult (Result Http.Error TxReceipt)
    | TxSigned TxInfo (Result String TxHash)
    | ViewDraft (Maybe Post.Draft)
    | BlockTimeFetched Int (Result Http.Error Time.Posix)
    | RestoreDraft Post.Draft
    | DismissNotice Int
    | ClickHappened
    | PostUXMsg PostUXId PostUX.Msg
    | ComposeUXMsg ComposeUX.Msg
    | HomeMsg Home.Msg
    | AllowanceFetched Address (Result Http.Error TokenValue)
    | BalanceFetched Address (Result Http.Error TokenValue)
    | MsgUp MsgUp


type Mode
    = BlankMode
    | Home Home.Model
    | Compose
    | ViewContext Post.Context


filterPosts : (Post.Published -> Bool) -> PublishedPostsDict -> PublishedPostsDict
filterPosts filterFunc =
    Dict.map
        (always <| List.filter filterFunc)
        >> Dict.filter
            (\_ publishedPosts ->
                if publishedPosts == [] then
                    False

                else
                    True
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
