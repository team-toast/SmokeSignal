module Misc exposing (decodeFaucetResponse, defaultSeoDescription, defaultTopic, dollarStringToToken, emptyComposeModel, emptyModel, formatDollar, formatPosix, getPostOrReply, getTitle, getTxReceipt, initDemoPhaceSrc, parseHttpError, postIdToKey, sortPostsFunc, sortTopics, sortTypeToString, tryRouteToView, validateTopic)

import Array
import Browser.Navigation
import Dict exposing (Dict)
import Eth.Decode
import Eth.Encode
import Eth.RPC
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Eth.Utils
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import GTag
import Helpers.Element
import Helpers.Time
import Http
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra exposing (unwrap)
import String.Extra
import Task exposing (Task)
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (..)


emptyModel : Browser.Navigation.Key -> Model
emptyModel key =
    { navKey = key
    , view = ViewHome
    , wallet = Types.NoneDetected
    , newUserModal = False
    , now = Time.millisToPosix 0
    , dProfile = Helpers.Element.Desktop
    , sentries =
        { xDai = Nothing
        , ethereum = Nothing
        }
    , blockTimes = Dict.empty
    , showAddressId = Nothing
    , userNotices = []
    , trackedTxs = Dict.empty
    , showExpandedTrackedTxs = False
    , demoPhaceSrc = initDemoPhaceSrc
    , cookieConsentGranted = False
    , maybeSeoDescription = Nothing
    , topicInput = ""
    , config = emptyConfig
    , compose = emptyComposeModel
    , postState = Nothing
    , tooltipState = Nothing
    , rootPosts = Dict.empty
    , replyPosts = Dict.empty
    , replyIds = Dict.empty
    , accounting = Dict.empty
    , topics = Dict.empty
    , alphaUrl = ""
    , pages = Array.empty
    , currentPage = 0
    , chainSwitchInProgress = False
    , faucetToken = ""
    , gtagHistory = GTag.emptyGtagHistory
    , sortType = HotSort
    }


emptyComposeModel : ComposeModel
emptyComposeModel =
    { title = ""
    , dollar = ""
    , body = ""
    , modal = False
    , donate = True
    , context = TopLevel defaultTopic
    , preview = False
    , inProgress = False
    , error = Nothing
    , message = Nothing
    }


emptyConfig : Config
emptyConfig =
    { xDai =
        { chain = Types.Eth
        , contract = emptyAddress
        , startScanBlock = 0
        , providerUrl = ""
        }
    , ethereum =
        { chain = Types.Eth
        , contract = emptyAddress
        , startScanBlock = 0
        , providerUrl = ""
        }
    }


emptyAddress : Address
emptyAddress =
    Eth.Utils.unsafeToAddress ""


initDemoPhaceSrc : String
initDemoPhaceSrc =
    "2222222222222222222222222228083888c8f222"


getTitle : Model -> String
getTitle model =
    let
        defaultMain =
            "SmokeSignal | Uncensorable - Immutable - Unkillable | Real Free Speech - Cemented on the Blockchain"
    in
    case model.view of
        ViewHome ->
            defaultMain

        ViewTopics ->
            defaultMain

        ViewPost postId ->
            Dict.get (postIdToKey postId) model.rootPosts
                |> Maybe.andThen (.core >> .content >> .title)
                |> unwrap defaultMain (\contextTitle -> contextTitle ++ " | SmokeSignal")

        ViewTopic topic ->
            "#" ++ topic ++ " | SmokeSignal"

        ViewWallet ->
            defaultMain

        ViewTxns ->
            defaultMain

        ViewAbout ->
            defaultMain

        ViewUser _ ->
            defaultMain


defaultSeoDescription : String
defaultSeoDescription =
    "SmokeSignal - Uncensorable, Global, Immutable chat. Burn crypto to cement your writing on the blockchain. Grant your ideas immortality."


formatPosix : Posix -> String
formatPosix t =
    [ [ Time.toYear Time.utc t
            |> String.fromInt
      , Time.toMonth Time.utc t
            |> Helpers.Time.monthToInt
            |> String.fromInt
            |> String.padLeft 2 '0'
      , Time.toDay Time.utc t
            |> String.fromInt
            |> String.padLeft 2 '0'
      ]
        |> String.join "-"
    , [ Time.toHour Time.utc t
            |> String.fromInt
            |> String.padLeft 2 '0'
      , Time.toMinute Time.utc t
            |> String.fromInt
            |> String.padLeft 2 '0'
      ]
        |> String.join ":"
    , "(UTC)"
    ]
        |> String.join " "


parseHttpError : Http.Error -> String
parseHttpError err =
    case err of
        Http.BadUrl _ ->
            "Bad Url"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus statusCode ->
            "Status Code: " ++ String.fromInt statusCode

        Http.BadBody e ->
            e


tryRouteToView : Route -> Result String View
tryRouteToView route =
    case route of
        RouteHome ->
            Ok ViewHome

        RouteWallet ->
            Ok ViewWallet

        RouteTxns ->
            Ok ViewTxns

        RouteTopics ->
            Ok ViewTopics

        RouteAbout ->
            Ok ViewAbout

        RouteUser addr ->
            Ok <| ViewUser addr

        RouteViewPost postId ->
            Ok <| ViewPost postId

        RouteTopic topic ->
            topic
                |> validateTopic
                |> Maybe.map ViewTopic
                |> Result.fromMaybe "Malformed topic"

        RouteInvalid ->
            Err "Path not found"


postIdToKey : PostId -> PostKey
postIdToKey id =
    ( String.fromInt id.block, Eth.Utils.hexToString id.messageHash )


formatDollar : TokenValue -> String
formatDollar =
    TokenValue.toFloatWithWarning
        >> formatFloat 2


dollarStringToToken : Float -> String -> Maybe TokenValue
dollarStringToToken ethPrice =
    String.toFloat
        >> Maybe.map
            (\dollarValue ->
                TokenValue.fromFloatWithWarning (dollarValue / ethPrice)
            )


sortTopics : Dict String Types.Count -> List ( String, Types.Count )
sortTopics =
    Dict.toList
        >> List.sortBy
            (Tuple.second
                >> .total
                >> TokenValue.toFloatWithWarning
                >> negate
            )


validateTopic : String -> Maybe String
validateTopic =
    String.toLower
        >> String.map
            (\c ->
                if Char.isAlphaNum c then
                    c

                else
                    ' '
            )
        >> String.Extra.clean
        >> String.replace " " "-"
        >> (\str ->
                if String.isEmpty str then
                    Nothing

                else
                    Just str
           )


getPostOrReply : PostId -> Model -> Maybe Core
getPostOrReply id model =
    let
        key =
            postIdToKey id
    in
    model.rootPosts
        |> Dict.get key
        |> Maybe.Extra.unwrap
            (model.replyPosts
                |> Dict.get key
                |> Maybe.map .core
            )
            (.core >> Just)


{-| Version of Eth.getTxReceipt that handles the normal outcome of a null response
if the Tx has not been mined yet.
<https://github.com/cmditch/elm-ethereum/blob/5.0.0/src/Eth.elm#L225>
-}
getTxReceipt : String -> TxHash -> Task Http.Error (Maybe TxReceipt)
getTxReceipt url txHash =
    Eth.RPC.toTask
        { url = url
        , method = "eth_getTransactionReceipt"
        , params = [ Eth.Encode.txHash txHash ]
        , decoder = Decode.nullable Eth.Decode.txReceipt
        }


sortPostsFunc : SortType -> Dict Int Time.Posix -> Dict PostKey Accounting -> Time.Posix -> (Core -> Float)
sortPostsFunc sortType blockTimes accounting now =
    let
        postTimeDefaultZero post =
            blockTimes
                |> Dict.get post.id.block
                |> Maybe.withDefault (Time.millisToPosix 0)

        ageOf post =
            Helpers.Time.sub now (postTimeDefaultZero post)

        ageFactor post =
            -- 1 at age zero, falls to 0 when 90 days old
            Helpers.Time.getRatio
                (ageOf post)
                (Helpers.Time.mul Helpers.Time.oneDay 90)
                |> clamp 0 1
                |> (\ascNum -> 1 - ascNum)

        totalBurned post =
            accounting
                |> Dict.get post.key
                |> unwrap
                    post.authorBurn
                    .totalBurned
                |> TokenValue.toFloatWithWarning

        newnessMultiplier post =
            (ageFactor post * 4.0) + 1
    in
    case sortType of
        BurnSort ->
            totalBurned
                >> negate

        HotSort ->
            \post ->
                totalBurned post
                    * newnessMultiplier post
                    |> negate

        NewSort ->
            \post ->
                ageOf post
                    |> Time.posixToMillis
                    |> toFloat


sortTypeToString : SortType -> String
sortTypeToString sortType =
    case sortType of
        HotSort ->
            "Hot"

        BurnSort ->
            "Burn"

        NewSort ->
            "New"


decodeFaucetResponse : Decoder FaucetResult
decodeFaucetResponse =
    Decode.map2 FaucetResult
        (Decode.field "status" Decode.bool)
        (Decode.field "message" Decode.string)


defaultTopic : String
defaultTopic =
    "misc"


formatFloat : Int -> Float -> String
formatFloat numDecimals =
    FormatNumber.format
        { usLocale
            | decimals = FormatNumber.Locales.Exact numDecimals
        }
