module Misc exposing (contextReplyTo, contextTopLevel, decodePostData, defaultSeoDescription, defaultTopic, emptyModel, encodeDraft, formatPosix, getPublishedPostFromId, getTitle, justBodyContent, nullMetadata, totalBurned, txInfoToNameStr, updatePublishedPost, withBalance)

import Browser.Navigation
import Dict
import Eth.Sentry.Event
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Hex, TxHash)
import Eth.Utils
import Helpers.Element
import Helpers.Time
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Maybe.Extra exposing (unwrap)
import Ports
import Result.Extra
import String.Extra
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (Content, Context(..), Draft, EncodedDraft, Id, Metadata, Model, Msg(..), Post(..), Published, PublishedPostsDict, TrackedTx, TxInfo(..), UserInfo, View(..))


emptyModel : Browser.Navigation.Key -> Model
emptyModel key =
    { navKey = key
    , basePath = ""
    , route = Types.Home
    , wallet = Types.NoneDetected
    , newUserModal = False
    , now = Time.millisToPosix 0
    , dProfile = Helpers.Element.Desktop
    , txSentry =
        TxSentry.init
            ( Ports.txOut, Ports.txIn )
            TxSentryMsg
            ""
    , eventSentry = Eth.Sentry.Event.init (always Types.ClickHappened) "" |> Tuple.first
    , publishedPosts = Dict.empty

    --, postUX = Nothing
    , replies = []
    , view = ViewHome
    , showHalfComposeUX = False

    --, composeUXModel = Nothing -- TODO ComposeUX.init now (TopLevel Post.defaultTopic)
    , blockTimes = Dict.empty
    , showAddressId = Nothing
    , userNotices = []
    , trackedTxs = []
    , showExpandedTrackedTxs = False
    , draftModal = Nothing
    , demoPhaceSrc = ""
    , donateChecked = True
    , cookieConsentGranted = False
    , maybeSeoDescription = Nothing
    , searchInput = ""
    , composeModal = False
    , config = Types.ConfigData "" "" ""

    --, topicUXModel = Nothing
    }


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

        ViewCompose ->
            "Compose | SmokeSignal"

        ViewPost postId ->
            getPublishedPostFromId model.publishedPosts postId
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


getPublishedPostFromId :
    PublishedPostsDict
    -> Id
    -> Maybe Published
getPublishedPostFromId publishedPosts postId =
    publishedPosts
        |> Dict.get postId.block
        |> Maybe.map
            (List.filter
                (\post ->
                    post.id.messageHash == postId.messageHash
                )
            )
        |> Maybe.andThen List.head


getPublishedPostFromTxHash :
    PublishedPostsDict
    -> TxHash
    -> Maybe Published
getPublishedPostFromTxHash publishedPosts txHash =
    publishedPosts
        |> Dict.values
        |> List.concat
        |> List.filter
            (\publishedPost ->
                publishedPost.txHash == txHash
            )
        |> List.head



--viewContextToMaybeDescription :
--PublishedPostsDict
---> ViewContext
---> Maybe String
--viewContextToMaybeDescription posts context =
--case context of
--ViewPost postId ->
--getPublishedPostFromId posts postId
--|> Maybe.andThen (.core >> .content >> .desc)
--Topic topic ->
--Just <| "Discussions related to #" ++ topic ++ " on SmokeSignal"
--postContextToViewContext :
--Context
---> ViewContext
--postContextToViewContext postContext =
--case postContext of
--Reply id ->
--ViewPost id
--TopLevel topicStr ->
--Topic topicStr
--viewContextToPostContext :
--ViewContext
---> Context
--viewContextToPostContext viewContext =
--case viewContext of
--ViewPost id ->
--Reply id
--Topic topicStr ->
--TopLevel topicStr


defaultSeoDescription : String
defaultSeoDescription =
    "SmokeSignal - Uncensorable, Global, Immutable chat. Burn crypto to cement your writing on the blockchain. Grant your ideas immortality."


updatePublishedPost :
    Id
    -> (Published -> Published)
    -> PublishedPostsDict
    -> PublishedPostsDict
updatePublishedPost postId updateFunc posts =
    posts
        |> Dict.update postId.block
            (Maybe.map <|
                List.map
                    (\thisPost ->
                        if thisPost.id == postId then
                            updateFunc thisPost

                        else
                            thisPost
                    )
            )


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


justBodyContent : String -> Content
justBodyContent =
    Content Nothing Nothing


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
        Reply _ ->
            Nothing

        TopLevel topic ->
            Just topic


contextReplyTo : Context -> Maybe Id
contextReplyTo context =
    case context of
        Reply id ->
            Just id

        _ ->
            Nothing


defaultTopic : String
defaultTopic =
    "misc"


nullMetadata : Metadata
nullMetadata =
    Metadata
        0
        (TopLevel defaultTopic)
        Nothing


currentMetadataVersion : Int
currentMetadataVersion =
    3


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


decodePostData : String -> ( Metadata, Content )
decodePostData src =
    src
        |> D.decodeString postDataDecoder
        |> Result.Extra.extract
            (\decodeErr ->
                ( { nullMetadata
                    | maybeDecodeError =
                        Just <|
                            D.errorToString decodeErr
                  }
                , justBodyContent src
                )
            )


postDataDecoder : D.Decoder ( Metadata, Content )
postDataDecoder =
    metadataDecoder
        |> D.andThen
            (\metadata ->
                D.map
                    (Tuple.pair metadata)
                    (messageDataDecoder metadata.metadataVersion)
            )


metadataDecoder : D.Decoder Metadata
metadataDecoder =
    versionDecoder
        |> D.andThen versionedMetadataDecoder


versionDecoder : D.Decoder Int
versionDecoder =
    D.maybe
        (D.field "v" D.int)
        |> D.map (Maybe.withDefault 1)


versionedMetadataDecoder : Int -> D.Decoder Metadata
versionedMetadataDecoder version =
    case version of
        0 ->
            D.succeed nullMetadata

        1 ->
            D.maybe (D.field "re" postIdDecoder)
                |> D.map
                    (\maybeReplyTo ->
                        case maybeReplyTo of
                            Just replyTo ->
                                Metadata
                                    version
                                    (Reply replyTo)
                                    Nothing

                            Nothing ->
                                Metadata
                                    version
                                    (TopLevel <| defaultTopic)
                                    Nothing
                    )

        2 ->
            D.field "c" contextDecoder
                |> D.map
                    (\context ->
                        Metadata
                            version
                            context
                            Nothing
                    )

        3 ->
            currentMetadataDecoder version

        _ ->
            currentMetadataDecoder version
                |> D.map
                    (\metadata ->
                        { metadata
                            | maybeDecodeError =
                                Just <| "Unknown metadata version '" ++ String.fromInt version ++ "'. Decoding for version '" ++ String.fromInt currentMetadataVersion ++ "'."
                        }
                    )


currentMetadataDecoder : Int -> D.Decoder Metadata
currentMetadataDecoder decodedVersion =
    D.field "c" contextDecoder
        |> D.map
            (\context ->
                Metadata
                    decodedVersion
                    context
                    Nothing
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


contextDecoder : D.Decoder Context
contextDecoder =
    D.oneOf
        [ D.map Reply <| D.field "re" postIdDecoder
        , D.map TopLevel <| D.field "topic" D.string
        ]


encodePostId : Id -> E.Value
encodePostId postId =
    E.list identity
        [ E.int postId.block
        , encodeHex postId.messageHash
        ]


postIdDecoder : D.Decoder Id
postIdDecoder =
    D.map2
        Id
        (D.index 0 D.int)
        (D.index 1 hexDecoder)


encodeContent : Content -> E.Value
encodeContent content =
    E.list identity
        [ E.string <| Maybe.withDefault "" <| content.title
        , E.string <| Maybe.withDefault "" <| content.desc
        , E.string content.body
        ]


messageDataDecoder : Int -> D.Decoder Content
messageDataDecoder metadataVersion =
    D.field "m" <|
        if metadataVersion < 3 then
            D.map justBodyContent D.string

        else
            D.map3
                Content
                (D.index 0 <| D.map String.Extra.nonEmpty <| D.string)
                (D.index 1 <| D.map String.Extra.nonEmpty <| D.string)
                (D.index 2 D.string)


encodeHex : Hex -> E.Value
encodeHex =
    Eth.Utils.hexToString >> E.string


hexDecoder : D.Decoder Hex
hexDecoder =
    D.string
        |> D.map Eth.Utils.toHex
        |> D.andThen
            (\result ->
                case result of
                    Err errStr ->
                        D.fail errStr

                    Ok hex ->
                        D.succeed hex
            )


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
