module Types exposing (..)

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
import Post exposing (Post, PublishedPost)
import Routing exposing (Route)
import Time
import TokenValue exposing (TokenValue)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import Wallet exposing (Wallet)


type alias Flags =
    { networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , route : Route
    , wallet : Wallet
    , now : Time.Posix
    , dProfile : EH.DisplayProfile
    , txSentry : TxSentry Msg
    , eventSentry : EventSentry Msg
    , publishedPosts : PublishedPostsDict
    , replies : List Reply
    , mode : Mode
    , showHalfComposeUX : Bool
    , replyTo : Maybe Post.Id
    , composeUXModel : ComposeUX.Model
    , blockTimes : Dict Int Time.Posix
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : List TrackedTx -- Can't use TxHash as a key; Elm is silly with what is and is not comparable
    , showExpandedTrackedTxs : Bool
    , draftModal : Maybe Post.Draft
    , demoPhaceSrc : String
    }


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
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
    | ShowExpandedTrackedTxs Bool
    | CheckTrackedTxsStatus
    | TrackedTxStatusResult (Result Http.Error TxReceipt)
    | TxSigned TxInfo (Result String TxHash)
    | TxMined TxInfo (Result String TxReceipt)
    | ViewDraft (Maybe Post.Draft)
    | BlockTimeFetched Int (Result Http.Error Time.Posix)
    | UpdateReplyTo (Maybe Post.Id)
    | DismissNotice Int
    | ClickHappened
    | ComposeUXMsg ComposeUX.Msg
    | HomeMsg Home.Msg
    | AllowanceFetched Address (Result Http.Error TokenValue)
    | BalanceFetched Address (Result Http.Error TokenValue)
    | MsgUp MsgUp


type Mode
    = BlankMode
    | Home Home.Model
    | Compose String
    | ViewPost Post.Id
    | ViewTopic String


filterBlockPosts : (PublishedPost -> Bool) -> PublishedPostsDict -> PublishedPostsDict
filterBlockPosts filterFunc =
    Dict.map
        (always <| List.filter filterFunc)
        >> Dict.filter
            (\_ publishedPosts ->
                if publishedPosts == [] then
                    False

                else
                    True
            )


updateTrackedTxStatusByTxInfo : TxInfo -> TxStatus -> Model -> Model
updateTrackedTxStatusByTxInfo txInfo newStatus model =
    { model
        | trackedTxs =
            model.trackedTxs
                |> List.map
                    (\trackedTx ->
                        if trackedTx.txInfo == txInfo then
                            { trackedTx
                                | status = newStatus
                            }

                        else
                            trackedTx
                    )
    }


updateTrackedTxStatus : TxHash -> TxStatus -> Model -> Model
updateTrackedTxStatus txHash newStatus model =
    { model
        | trackedTxs =
            model.trackedTxs
                |> List.map
                    (\trackedTx ->
                        if trackedTx.txHash == txHash then
                            { trackedTx
                                | status = newStatus
                            }

                        else
                            trackedTx
                    )
    }
