module Misc exposing (defaultSeoDescription, dollarStringToToken, emptyModel, formatDollar, formatPosix, getConfig, getPostOrReply, getPrice, getProviderUrl, getTitle, getTxReceipt, initDemoPhaceSrc, parseHttpError, postIdToKey, sortPosts, sortTopics, tokenToDollar, tryRouteToView, txInfoToNameStr, txUrl, validateTopic)

import Browser.Navigation
import Dict exposing (Dict)
import Eth.Decode
import Eth.Encode
import Eth.RPC
import Eth.Sentry.Event
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Eth.Utils
import FormatFloat
import Helpers.Element
import Helpers.Eth
import Helpers.Time
import Http
import Json.Decode as Decode
import Maybe.Extra exposing (unwrap)
import Ports
import Post
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
    , ethPrice = 1.0
    , xDaiPrice = 1.0
    , txSentry =
        TxSentry.init
            ( Ports.txOut, Ports.txIn )
            (TxSentryMsg XDai)
            ""
    , txSentryX =
        TxSentry.init
            ( Ports.txOut, Ports.txIn )
            (TxSentryMsg XDai)
            ""
    , sentries =
        { xDai =
            Eth.Sentry.Event.init (always Types.ClickHappened) "" |> Tuple.first
        , ethereum =
            Eth.Sentry.Event.init (always Types.ClickHappened) "" |> Tuple.first
        }
    , blockTimes = Dict.empty
    , showAddressId = Nothing
    , userNotices = []
    , trackedTxs = Dict.empty
    , showExpandedTrackedTxs = False
    , draftModal = Nothing
    , demoPhaceSrc = initDemoPhaceSrc
    , cookieConsentGranted = False
    , maybeSeoDescription = Nothing
    , topicInput = ""
    , config = emptyConfig
    , compose =
        { title = ""
        , dollar = ""
        , body = ""
        , modal = False
        , donate = True
        , context = TopLevel Post.defaultTopic
        }
    , tipOpen = Nothing
    , rootPosts = Dict.empty
    , replyPosts = Dict.empty
    , replyIds = Dict.empty
    , accounting = Dict.empty
    , topics = Dict.empty
    , hasNavigated = False
    , alphaUrl = ""
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

        ViewCompose _ ->
            "Compose | SmokeSignal"

        ViewPost postId ->
            Dict.get (postIdToKey postId) model.rootPosts
                |> Maybe.andThen (.core >> .content >> .title)
                |> unwrap defaultMain (\contextTitle -> contextTitle ++ " | SmokeSignal")

        ViewTopic topic ->
            "#" ++ topic ++ " | SmokeSignal"



-- postContextToViewContext :
--     Context
--     -> ViewContext
-- postContextToViewContext postContext =
--     case postContext of
--         Reply id ->
--             ViewPost id
--         TopLevel topicStr ->
--             Topic topicStr
-- viewContextToPostContext :
--     ViewContext
--     -> Context
-- viewContextToPostContext viewContext =
--     case viewContext of
--         ViewPost id ->
--             Reply id
--         Topic topicStr ->
--             TopLevel topicStr


defaultSeoDescription : String
defaultSeoDescription =
    "SmokeSignal - Uncensorable, Global, Immutable chat. Burn crypto to cement your writing on the blockchain. Grant your ideas immortality."


txInfoToNameStr : TxInfo -> String
txInfoToNameStr txInfo =
    case txInfo of
        PostTx _ ->
            "Post Submit"

        TipTx _ _ ->
            "Tip"

        BurnTx _ _ ->
            "Burn"


formatPosix : Posix -> String
formatPosix t =
    [ [ Time.toDay Time.utc t
            |> String.fromInt
            |> String.padLeft 2 '0'
      , Time.toMonth Time.utc t
            |> Helpers.Time.monthToInt
            |> String.fromInt
            |> String.padLeft 2 '0'
      , Time.toYear Time.utc t
            |> String.fromInt
            |> String.right 2
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

        RouteTopics ->
            Ok ViewTopics

        RouteViewPost postId ->
            Ok <| ViewPost postId

        RouteTopic topic ->
            topic
                |> validateTopic
                |> Maybe.map ViewTopic
                |> Result.fromMaybe "Malformed topic"

        RouteMalformedPostId ->
            Err "Malformed post ID"

        RouteInvalid ->
            Err "Path not found"


postIdToKey : PostId -> PostKey
postIdToKey id =
    ( String.fromInt id.block, Eth.Utils.hexToString id.messageHash )


tokenToDollar : Float -> TokenValue -> String
tokenToDollar eth tv =
    TokenValue.mulFloatWithWarning tv eth
        |> TokenValue.toFloatWithWarning
        |> FormatFloat.formatFloat 2


formatDollar : TokenValue -> String
formatDollar =
    TokenValue.toFloatWithWarning
        >> FormatFloat.formatFloat 2


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


getProviderUrl : Chain -> Config -> String
getProviderUrl chain =
    case chain of
        Eth ->
            .ethereum >> .providerUrl

        XDai ->
            .xDai >> .providerUrl


getConfig : Chain -> Config -> ChainConfig
getConfig chain =
    case chain of
        Eth ->
            .ethereum

        XDai ->
            .xDai


txUrl : Chain -> TxHash -> String
txUrl chain hash =
    case chain of
        Eth ->
            Helpers.Eth.etherscanTxUrl hash

        XDai ->
            "https://blockscout.com/poa/xdai/tx/"
                ++ Eth.Utils.txHashToString hash


getPrice : Chain -> Model -> Float
getPrice chain =
    case chain of
        Eth ->
            .ethPrice

        XDai ->
            .xDaiPrice


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


sortPosts : Dict Int Time.Posix -> Time.Posix -> Core -> Float
sortPosts blockTimes now post =
    let
        postTimeDefaultZero =
            blockTimes
                |> Dict.get post.id.block
                |> Maybe.withDefault (Time.millisToPosix 0)

        age =
            Helpers.Time.sub now postTimeDefaultZero

        ageFactor =
            -- 1 at age zero, falls to 0 when 3 days old
            Helpers.Time.getRatio
                age
                (Helpers.Time.mul Helpers.Time.oneDay 90)
                |> clamp 0 1
                |> (\ascNum -> 1 - ascNum)

        totalBurned =
            post.authorBurn
                |> TokenValue.toFloatWithWarning

        newnessMultiplier =
            (ageFactor * 4.0) + 1
    in
    totalBurned * newnessMultiplier
