module Types exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet exposing (WalletSentry)
import Eth.Types exposing (Address, Hex, TxHash, TxReceipt)
import Helpers.Element as EH
import Http
import Json.Decode
import Set exposing (Set)
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN exposing (UserNotice)


type alias Flags =
    { basePath : String
    , networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
    , cookieConsent : Bool
    , newUser : Bool
    , smokeSignalContractAddress : String
    , httpProviderUrl : String
    , startScanBlock : Int
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , basePath : String
    , wallet : Wallet
    , now : Time.Posix
    , dProfile : EH.DisplayProfile
    , txSentry : TxSentry Msg
    , eventSentry : EventSentry Msg
    , ethPrice : Float
    , view : View
    , blockTimes : Dict Int Time.Posix
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : Dict String TrackedTx -- Keyed by (Eth.Utils.txHashToString hash)
    , showExpandedTrackedTxs : Bool
    , draftModal : Maybe Draft
    , demoPhaceSrc : String
    , cookieConsentGranted : Bool
    , maybeSeoDescription : Maybe String
    , topicInput : String
    , newUserModal : Bool
    , tipOpen : Maybe PostState
    , config : Config
    , compose : ComposeModel
    , rootPosts : Dict PostKey RootPost
    , replyPosts : Dict PostKey ReplyPost
    , replyIds : Dict PostKey (Set PostKey)
    , accounting : Dict PostKey Accounting
    , topics : Dict String TokenValue
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | RouteChanged Route
    | Tick Time.Posix
    | EveryFewSeconds
    | ChangeDemoPhaceSrc
    | NewDemoSrc String
      -- | MutateDemoSrcWith MutateInfo
    | Resize Int Int
    | WalletStatus (Result String WalletSentry)
    | TxSentryMsg TxSentry.Msg
    | EventSentryMsg EventSentry.Msg
    | PostLogReceived (Eth.Types.Event (Result Json.Decode.Error LogPost))
    | PostAccountingFetched PostId (Result Http.Error Accounting)
    | ShowExpandedTrackedTxs Bool
    | CheckTrackedTxsStatus
    | TrackedTxStatusResult (Result Http.Error TxReceipt)
    | TxSigned TxInfo (Result String TxHash)
    | ViewDraft (Maybe Draft)
    | BlockTimeFetched Int (Result Http.Error Time.Posix)
      -- | RestoreDraft Draft
    | DismissNotice Int
    | ClickHappened
    | ComposeOpen
    | ComposeClose
    | BalanceFetched Address (Result Http.Error TokenValue)
    | CookieConsentGranted
    | StartInlineCompose Context
    | GotoView View
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | AddUserNotice UN.UserNotice
    | SubmitDraft
    | SubmitTip PostId
    | SubmitBurn PostId
    | DonationCheckboxSet Bool
    | ShowNewToSmokeSignalModal Bool
    | EthPriceFetched (Result Http.Error Float)
    | ComposeBodyChange String
    | ComposeTitleChange String
    | ComposeDollarChange String
    | TopicInputChange String
    | SetTipOpen PostState
    | CancelTipOpen


type alias PostKey =
    ( String, String )


type alias RootPost =
    { core : CoreData
    , topic : String
    }


type alias ReplyPost =
    { core : CoreData
    , parent : PostId
    }


type alias CoreData =
    { id : PostId
    , key : PostKey
    , txHash : TxHash
    , author : Address
    , authorBurn : TokenValue
    , content : Content
    , metadataVersion : Int
    }


type alias ComposeModel =
    { title : String
    , dollar : String
    , body : String
    , modal : Bool
    , donate : Bool
    , context : Context
    }


type alias Config =
    { smokeSignalContractAddress : Address
    , httpProviderUrl : String
    , startScanBlock : Int
    }


type alias PostState =
    { id : PostId
    , showInput : ShowInputState
    }


type PostUXId
    = PublishedPostId PostId
    | DraftPreview


type ShowInputState
    = Burn String
    | Tip String


type View
    = ViewHome
    | ViewCompose Context
    | ViewPost PostId
    | ViewTopic String
    | ViewTopics


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    , balance : Maybe TokenValue
    }


type Wallet
    = NoneDetected
    | OnlyNetwork Eth.Net.NetworkId
    | Active UserInfo


type LogPost
    = LogRoot RootPost
    | LogReply ReplyPost


type alias ReplyIds =
    { from : PostId
    , to : PostId
    }


type PhaceIconId
    = PhaceForPublishedPost PostId
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
    | TipTx PostId TokenValue
    | BurnTx PostId TokenValue


type TxStatus
    = Mining
    | Failed FailReason
    | Mined (Maybe PostId)


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
    , id : PostId
    , key : PostKey
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

    --, renderedPost : Element Never
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


type alias CheckedMaybeValidInputs =
    { content : Maybe Content
    , burnAndDonateAmount : Maybe (Result String ( TokenValue, TokenValue ))
    }


type Context
    = Reply PostId
    | TopLevel String


type alias PostId =
    { block : Int
    , messageHash : Hex
    }


type Route
    = RouteHome
    | RouteViewPost PostId
    | RouteMalformedPostId
    | RouteViewTopic String
    | RouteInvalid
    | RouteTopics
