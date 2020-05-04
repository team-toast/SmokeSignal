module Post exposing (..)

import Element exposing (Element)
import Element.Font
import ElementMarkdown
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.List as ListHelpers
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra
import Result.Extra
import String.Extra
import Theme exposing (Theme)
import TokenValue exposing (TokenValue)


type Post
    = PublishedPost Published
    | PostDraft Draft


type alias Published =
    { txHash : TxHash
    , id : Id
    , core : Core
    , crowdBurn : Maybe TokenValue
    , crowdTip : Maybe TokenValue
    }


type alias Draft =
    { donateAmount : TokenValue
    , core : Core
    }


getCore : Post -> Core
getCore post =
    case post of
        PublishedPost p ->
            p.core

        PostDraft d ->
            d.core


type alias Core =
    { from : Address
    , authorBurn : TokenValue
    , message : String
    , metadata : Metadata
    , renderedPost : Element Never
    }


type alias EncodedDraft =
    { author : Address
    , encodedMessageAndMetadata : String
    , burnAmount : TokenValue
    , donateAmount : TokenValue
    }


type alias Metadata =
    { metadataVersion : Int
    , context : Context
    , maybeDecodeError : Maybe String
    }


type Context
    = ForPost Id
    | ForTopic String


buildMetadataFromContext : Context -> Metadata
buildMetadataFromContext context =
    versionedMetadata <| Just context


contextReplyTo : Context -> Maybe Id
contextReplyTo context =
    case context of
        ForPost id ->
            Just id

        _ ->
            Nothing


contextTopic : Context -> Maybe String
contextTopic context =
    case context of
        ForTopic topic ->
            Just topic

        _ ->
            Nothing


type alias Id =
    { block : Int
    , messageHash : Hex
    }


defaultTopic =
    "misc"


nullMetadata : Metadata
nullMetadata =
    Metadata
        0
        (ForTopic defaultTopic)
        Nothing


currentMetadataVersion =
    2


versionedMetadata : Maybe Context -> Metadata
versionedMetadata maybeContext =
    Metadata
        currentMetadataVersion
        (maybeContext |> Maybe.withDefault (ForTopic defaultTopic))
        Nothing


blankVersionedMetadata : Metadata
blankVersionedMetadata =
    versionedMetadata Nothing


encodeDraft : Draft -> EncodedDraft
encodeDraft draft =
    EncodedDraft
        draft.core.from
        ("!smokesignal" ++ encodeMessageAndMetadataToString ( draft.core.message, draft.core.metadata ))
        draft.core.authorBurn
        draft.donateAmount


encodeMessageAndMetadataToString : ( String, Metadata ) -> String
encodeMessageAndMetadataToString ( message, metadata ) =
    E.encode 0
        (E.object <|
            [ ( "m", E.string message )
            , ( "v", E.int metadata.metadataVersion )
            , ( "c", encodeContext metadata.context )
            ]
        )


decodeMessageAndMetadata : String -> ( String, Metadata )
decodeMessageAndMetadata src =
    src
        |> D.decodeString messageDecoder
        |> Result.Extra.extract
            (\decodeErr ->
                ( src
                , { nullMetadata
                    | maybeDecodeError =
                        Just <|
                            D.errorToString decodeErr
                  }
                )
            )


messageDecoder : D.Decoder ( String, Metadata )
messageDecoder =
    D.map2
        Tuple.pair
        (D.field "m" D.string)
        metadataDecoder


metadataDecoder : D.Decoder Metadata
metadataDecoder =
    versionDecoder
        |> D.andThen
            (\v -> versionedMetadataDecoder v)


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
                                    (ForPost replyTo)
                                    Nothing

                            Nothing ->
                                Metadata
                                    version
                                    (ForTopic defaultTopic)
                                    Nothing
                    )

        2 ->
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
        ForPost postId ->
            E.object
                [ ( "re", encodePostId postId ) ]

        ForTopic topic ->
            E.object
                [ ( "topic", E.string topic ) ]


contextDecoder : D.Decoder Context
contextDecoder =
    D.oneOf
        [ D.map ForPost <| D.field "re" postIdDecoder
        , D.map ForTopic <| D.field "topic" D.string
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


encodeTopicList : List String -> E.Value
encodeTopicList topics =
    E.list
        (String.Extra.clean >> E.string)
        topics


topicListDecoder : D.Decoder (List String)
topicListDecoder =
    D.list
        (D.map sanitizeTopic D.string)


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


sanitizeTopic : String -> String
sanitizeTopic =
    String.toLower >> String.Extra.clean >> String.replace " " "-"
