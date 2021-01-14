module Common.Types exposing (..)

import Dict exposing (Dict)
import Element exposing (Element)
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, Hex, TxHash)
import Json.Decode
import Json.Encode
import TokenValue exposing (TokenValue)


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
