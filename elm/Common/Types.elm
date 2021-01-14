module Common.Types exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Element exposing (Element)
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet exposing (WalletSentry)
import Eth.Types exposing (Address, Hex, TxHash, TxReceipt)
import Helpers.Element as EH
import Http
import Json.Decode
import Json.Encode
import Time
import TokenValue exposing (TokenValue)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)


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
    = PublishedPostId Id
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
      --| PostUXMsg PostUXId PostUX.Msg
      --| ComposeUXMsg ComposeUX.Msg
      --| TopicUXMsg TopicUX.Msg
      --| HomeMsg Home.Msg
    | AllowanceFetched Address (Result Http.Error TokenValue)
    | BalanceFetched Address (Result Http.Error TokenValue)
    | CookieConsentGranted
    | MsgUp MsgUp


type Mode
    = BlankMode
      --| ModeHome Home.Model
    | ModeCompose
    | ViewContext ViewContext


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    , balance : Maybe TokenValue
    , unlockStatus : UnlockStatus
    }


type Wallet
    = NoneDetected
    | OnlyNetwork Eth.Net.NetworkId
    | Active UserInfo


type UnlockStatus
    = NotConnected
    | Checking
    | Locked
    | Unlocking
    | Unlocked


type ViewContext
    = ViewPost Id
    | Topic String


type alias PublishedPostsDict =
    Dict Int (List Published)


type alias ReplyIds =
    { from : Id
    , to : Id
    }


type MsgUp
    = StartInlineCompose Context
    | ExitCompose
    | GotoRoute Route
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | AddUserNotice UN.UserNotice
    | UnlockDai
    | SubmitPost Draft
    | SubmitTip Id TokenValue
    | SubmitBurn Id TokenValue
    | DonationCheckboxSet Bool


type MsgDown
    = UpdateWallet Wallet


type Route
    = Home
    | Compose Context
    | RouteViewContext ViewContext
    | NotFound String


type PhaceIconId
    = PhaceForPublishedPost Id
    | PhaceForDraft
    | PhaceForPreview
    | UserPhace
    | DemoPhace


type alias TrackedTx =
    { txHash : TxHash
    , txInfo : TxInfo
    , status : TxStatus
    }


type TxInfo
    = PostTx Draft
    | UnlockTx
    | TipTx Id TokenValue
    | BurnTx Id TokenValue


type TxStatus
    = Mining
    | Failed FailReason
    | Mined (Maybe Id)


type FailReason
    = MinedButExecutionFailed


type alias GTagData =
    { event : String
    , category : String
    , label : String
    , value : Int
    }


type Post
    = PublishedPost Published
    | PostDraft Draft


type alias Accounting =
    { firstAuthor : Address
    , totalBurned : TokenValue
    , totalTipped : TokenValue
    }


type alias Published =
    { txHash : TxHash
    , id : Id
    , core : Core
    , maybeAccounting : Maybe Accounting
    }


type alias Draft =
    { donateAmount : TokenValue
    , core : Core
    }


type alias Core =
    { author : Address
    , authorBurn : TokenValue
    , content : Content
    , metadata : Metadata
    , renderedPost : Element Never
    }


type alias Content =
    { title : Maybe String
    , desc : Maybe String
    , body : String
    }


type alias EncodedDraft =
    { author : Address
    , encodedContentAndMetadata : String
    , burnAmount : TokenValue
    , donateAmount : TokenValue
    }


type alias Metadata =
    { metadataVersion : Int
    , context : Context
    , maybeDecodeError : Maybe String
    }


type Context
    = Reply Id
    | TopLevel String


type alias Id =
    { block : Int
    , messageHash : Hex
    }
