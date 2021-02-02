module Misc exposing (contextReplyTo, contextTopLevel, defaultSeoDescription, emptyModel, encodeContent, encodeContext, encodeDraft, encodeHex, encodePostId, encodeToString, formatPosix, getTitle, initDemoPhaceSrc, parseHttpError, postIdToKey, totalBurned, tryRouteToView, txInfoToNameStr, updateTrackedTxByTxHash, updateTrackedTxByTxInfo, updateTrackedTxIf, withBalance)

import Browser.Navigation
import Dict
import Eth.Sentry.Event
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Hex, TxHash)
import Eth.Utils
import Helpers.Element
import Helpers.Time
import Http
import Json.Encode as E
import List.Extra
import Maybe.Extra exposing (unwrap)
import Ports
import Set
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (..)


emptyModel : Browser.Navigation.Key -> Model
emptyModel key =
    { navKey = key
    , basePath = ""
    , view = ViewHome
    , wallet = Types.NoneDetected
    , newUserModal = False
    , now = Time.millisToPosix 0
    , dProfile = Helpers.Element.Desktop
    , ethPrice = Nothing
    , txSentry =
        TxSentry.init
            ( Ports.txOut, Ports.txIn )
            TxSentryMsg
            ""
    , eventSentry = Eth.Sentry.Event.init (always Types.ClickHappened) "" |> Tuple.first
    , blockTimes = Dict.empty
    , showAddressId = Nothing
    , userNotices = []
    , trackedTxs = []
    , showExpandedTrackedTxs = False
    , draftModal = Nothing
    , demoPhaceSrc = initDemoPhaceSrc
    , cookieConsentGranted = False
    , maybeSeoDescription = Nothing
    , searchInput = ""
    , config =
        Types.Config
            (Eth.Utils.unsafeToAddress "")
            ""
            0
    , compose =
        { title = ""
        , dai = ""
        , body = ""
        , modal = False
        , donate = False
        }
    , rootPosts = Dict.empty
    , replyPosts = Dict.empty
    , replyIds = Dict.empty
    , accounting = Dict.empty
    , topics = Set.empty
    }


initDemoPhaceSrc : String
initDemoPhaceSrc =
    "2222222222222222222222222228083888c8f222"


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
    case model.view of
        ViewHome ->
            defaultMain

        ViewCompose _ ->
            "Compose | SmokeSignal"

        ViewPost postId ->
            Dict.get (postIdToKey postId) model.rootPosts
                |> Maybe.andThen (.core >> .content >> .title)
                |> unwrap defaultMain (\contextTitle -> contextTitle ++ " | SmokeSignal")

        ViewTopic topic ->
            "#" ++ topic ++ " | SmokeSignal"


withBalance :
    TokenValue
    -> UserInfo
    -> UserInfo
withBalance balance userInfo =
    { userInfo
        | balance = Just balance
    }



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
        UnlockTx ->
            "Unlock DAI"

        PostTx _ ->
            "Post Submit"

        TipTx postId amount ->
            "Tip"

        BurnTx postId amount ->
            "Burn"


totalBurned : Post -> TokenValue
totalBurned post =
    case post of
        PublishedPost publishedPost ->
            case publishedPost.maybeAccounting of
                Just accounting ->
                    accounting.totalBurned

                Nothing ->
                    publishedPost.core.authorBurn

        PostDraft postDraft ->
            postDraft.core.authorBurn


contextTopLevel : Context -> Maybe String
contextTopLevel context =
    case context of
        TopLevel topic ->
            Just topic

        _ ->
            Nothing


contextReplyTo : Context -> Maybe PostId
contextReplyTo context =
    case context of
        Reply id ->
            Just id

        _ ->
            Nothing


encodeDraft : Draft -> EncodedDraft
encodeDraft draft =
    EncodedDraft
        draft.core.author
        ("!smokesignal" ++ encodeToString ( draft.core.metadata, draft.core.content ))
        draft.core.authorBurn
        draft.donateAmount


encodeToString : ( Metadata, Content ) -> String
encodeToString ( metadata, content ) =
    E.encode 0
        (E.object <|
            [ ( "m", encodeContent content )
            , ( "v", E.int metadata.metadataVersion )
            , ( "c", encodeContext metadata.context )
            ]
        )


encodeContext : Context -> E.Value
encodeContext context =
    case context of
        Reply postId ->
            E.object
                [ ( "re", encodePostId postId ) ]

        TopLevel topic ->
            E.object
                [ ( "topic", E.string topic ) ]


encodePostId : PostId -> E.Value
encodePostId postId =
    E.list identity
        [ E.int postId.block
        , encodeHex postId.messageHash
        ]


encodeContent : Content -> E.Value
encodeContent content =
    E.list identity
        [ E.string <| Maybe.withDefault "" <| content.title
        , E.string <| Maybe.withDefault "" <| content.desc
        , E.string content.body
        ]


encodeHex : Hex -> E.Value
encodeHex =
    Eth.Utils.hexToString >> E.string


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
    , [ Time.toMinute Time.utc t
            |> String.fromInt
            |> String.padLeft 2 '0'
      , Time.toHour Time.utc t
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

        RouteViewPost postId ->
            Ok <| ViewPost postId

        RouteViewTopic topic ->
            Ok <| ViewTopic topic

        RouteMalformedPostId ->
            Err "Malformed post ID"

        RouteMalformedTopic ->
            Err "Malformed topic"

        RouteInvalid ->
            Err "Path not found"


postIdToKey : PostId -> PostKey
postIdToKey id =
    ( String.fromInt id.block, Eth.Utils.hexToString id.messageHash )
