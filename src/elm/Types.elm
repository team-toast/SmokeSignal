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
    , newUser : Bool
    , smokeSignalContractAddress : String
    , httpProviderUrl : String
    , startScanBlock : Int
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , basePath : String
    , view : View
    , wallet : Wallet
    , now : Time.Posix
    , dProfile : EH.DisplayProfile
    , txSentry : TxSentry Msg
    , eventSentry : EventSentry Msg
    , publishedPosts : PublishedPostsDict
    , ethPrice : Maybe Float
    , replies : List ReplyIds
    , view : View
    , showHalfComposeUX : Bool
    , blockTimes : Dict Int Time.Posix
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : List TrackedTx -- Can't use TxHash as a key; Elm is silly with what is and is not comparable
    , showExpandedTrackedTxs : Bool
    , draftModal : Maybe Draft
    , demoPhaceSrc : String
    , cookieConsentGranted : Bool
    , maybeSeoDescription : Maybe String
    , searchInput : String
    , newUserModal : Bool
    , config : Config
    , compose : ComposeModel
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
    | PostLogReceived Eth.Types.Log
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
    | ComposeToggle
    | BalanceFetched Address (Result Http.Error TokenValue)
    | CookieConsentGranted
    | StartInlineCompose Context
    | ExitCompose
    | GotoView View
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | AddUserNotice UN.UserNotice
    | UnlockDai
    | SubmitDraft
    | SubmitPost Draft
    | SubmitTip PostId TokenValue
    | SubmitBurn PostId TokenValue
    | DonationCheckboxSet Bool
    | ShowNewToSmokeSignalModal Bool
    | EthPriceFetched (Result Http.Error Float)
    | ComposeBodyChange String
    | ComposeTitleChange String
    | ComposeDaiChange String


type alias ComposeModel =
    { title : String
    , dai : String
    , body : String
    , modal : Bool
    , donate : Bool
    }


type alias Config =
    { smokeSignalContractAddress : Address
    , httpProviderUrl : String
    , startScanBlock : Int
    }


type alias PostState =
    { showAddress : Bool
    , showInput : ShowInputState
    }


type PostUXId
    = PublishedPostId PostId
    | DraftPreview


type ShowInputState
    = None
    | Burn String
    | Tip String


type View
    = ViewHome
    | ViewCompose Context
    | ViewPost PostId
    | ViewTopic String


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    , balance : Maybe TokenValue
    }


type Wallet
    = NoneDetected
    | OnlyNetwork Eth.Net.NetworkId
    | Active UserInfo


type alias PublishedPostsDict =
    Dict Int (List Published)


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
    | UnlockTx
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
    | RouteMalformedTopic
    | RouteInvalid
