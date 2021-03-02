module Types exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet exposing (WalletSentry)
import Eth.Types exposing (Address, Hex, TxHash, TxReceipt)
import Helpers.Element as EH
import Http
import Json.Decode exposing (Value)
import Set exposing (Set)
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN exposing (UserNotice)


type alias Flags =
    { width : Int
    , height : Int
    , nowInMillis : Int
    , cookieConsent : Bool
    , newUser : Bool
    , ethProviderUrl : String
    , xDaiProviderUrl : String
    , hasWallet : Bool
    , chains : Value
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , wallet : Wallet
    , now : Time.Posix
    , dProfile : EH.DisplayProfile
    , txSentry : TxSentry Msg
    , txSentryX : TxSentry Msg
    , sentries :
        { xDai : EventSentry Msg
        , ethereum : EventSentry Msg
        }
    , ethPrice : Float
    , xDaiPrice : Float
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
    , topics : Dict String Count
    , hasNavigated : Bool
    , alphaUrl : String
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
    | TxSentryMsg Chain TxSentry.Msg
    | EventSentryMsg Chain EventSentry.Msg
    | PostLogReceived (Eth.Types.Event (Result Json.Decode.Error LogPost))
    | PostAccountingFetched PostId (Result Http.Error Accounting)
    | ShowExpandedTrackedTxs Bool
    | CheckTrackedTxsStatus
    | TrackedTxStatusResult (Result Http.Error (Maybe TxReceipt))
    | TxSigned Chain TxInfo (Result String TxHash)
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
    | XDaiPriceFetched (Result Http.Error Float)
    | ComposeBodyChange String
    | ComposeTitleChange String
    | ComposeDollarChange String
    | TopicInputChange String
    | SetTipOpen PostState
    | CancelTipOpen
    | GoBack
    | WalletResponse WalletConnectResponse
    | RpcResponse (Result Http.Error UserInfo)


type alias PostKey =
    ( String, String )


type alias RootPost =
    { core : Core
    , topic : String
    }


type alias ChainConfig =
    { chain : Chain
    , contract : Address
    , startScanBlock : Int
    , providerUrl : String
    }


type alias ReplyPost =
    { core : Core
    , parent : PostId
    }


type alias Count =
    { ids : Set PostKey
    , total : TokenValue
    }


type alias Core =
    { id : PostId
    , key : PostKey
    , txHash : TxHash
    , author : Address
    , authorBurn : TokenValue
    , content : Content
    , metadataVersion : Int
    , chain : Chain
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
    { xDai : ChainConfig
    , ethereum : ChainConfig
    }


type alias PostState =
    { id : PostId
    , showInput : ShowInputState
    }


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
    { address : Address
    , balance : TokenValue
    , chain : Chain
    }


type alias WalletInfo =
    { walletSentry : WalletSentry
    , balance : Maybe TokenValue
    }


type WalletConnectResponse
    = WalletSucceed UserInfo
    | WalletCancel
    | WalletInProgress
    | WalletError


type Wallet
    = NoneDetected
    | NetworkReady
    | Connecting
    | Active UserInfo


type LogPost
    = LogRoot RootPost
    | LogReply ReplyPost


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
    , chain : Chain
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


type alias Accounting =
    { firstAuthor : Address
    , totalBurned : TokenValue
    , totalTipped : TokenValue
    }


type alias Draft =
    { donateAmount : TokenValue
    , author : Address
    , authorBurn : TokenValue
    , content : Content
    , metadata : Metadata
    }


type alias Content =
    { title : Maybe String
    , desc : Maybe String
    , body : String
    }


type alias Metadata =
    { metadataVersion : Int
    , context : Context
    , maybeDecodeError : Maybe String
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


type Chain
    = XDai
    | Eth
