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
import Theme exposing (Theme)
import TokenValue exposing (TokenValue)


type alias Post =
    { from : Address
    , burnAmount : TokenValue
    , message : String
    , metadata : Metadata
    }


type alias Draft =
    { donateAmount : TokenValue
    , post : Post
    }


type alias PublishedPost =
    { txHash : TxHash
    , id : Id
    , post : Post
    }


type alias EncodedDraft =
    { author : Address
    , encodedMessageAndMetadata : String
    , burnAmount : TokenValue
    , donateAmount : TokenValue
    }


type alias Metadata =
    { metadataVersion : Int
    , replyTo : Maybe Id
    , topic : String
    , maybeDecodeError : Maybe D.Error
    }


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
        Nothing
        defaultTopic
        Nothing


currentMetadataVersion =
    2


versionedMetadata : Maybe Id -> Maybe String -> Metadata
versionedMetadata maybeReply maybeTopic =
    Metadata
        currentMetadataVersion
        maybeReply
        (maybeTopic |> Maybe.withDefault defaultTopic)
        Nothing


blankVersionedMetadata : Metadata
blankVersionedMetadata =
    versionedMetadata Nothing Nothing


encodeDraft : Draft -> EncodedDraft
encodeDraft draft =
    EncodedDraft
        draft.post.from
        ("!smokesignal" ++ encodeMessageAndMetadataToString ( draft.post.message, draft.post.metadata ))
        draft.post.burnAmount
        draft.donateAmount


encodeMessageAndMetadataToString : ( String, Metadata ) -> String
encodeMessageAndMetadataToString ( message, metadata ) =
    E.encode 0
        (E.object <|
            Maybe.Extra.values <|
                [ Just ( "m", E.string message )
                , Just ( "v", E.int metadata.metadataVersion )
                , Maybe.map (Tuple.pair "re") <|
                    Maybe.map encodePostId metadata.replyTo
                , if metadata.topic == defaultTopic then
                    Nothing

                  else
                    Just <| Tuple.pair "topic" <| E.string metadata.topic
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
                    | maybeDecodeError = Just decodeErr
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
    D.map4
        Metadata
        (D.maybe
            (D.field "v" D.int)
            |> D.map (Maybe.withDefault 1)
        )
        (D.maybe (D.field "re" postIdDecoder))
        (D.maybe (D.field "topic" D.string)
            |> D.map (Maybe.map String.toLower)
            |> D.map (Maybe.withDefault defaultTopic)
        )
        (D.succeed Nothing)


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
    E.list E.string topics


topicListDecoder : D.Decoder (List String)
topicListDecoder =
    D.list D.string


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


renderContentOrError : Theme msg -> String -> Element msg
renderContentOrError theme content =
    let
        renderResult =
            ElementMarkdown.renderString
                [ Element.spacing 15
                , Element.Font.color theme.postBodyTextColor
                ]
                content
    in
    case renderResult of
        Ok rendered ->
            rendered

        Err errStr ->
            Element.el
                [ Element.Font.color theme.errorTextColor
                , Element.Font.italic
                ]
            <|
                Element.text <|
                    "Error parsing/rendering markdown: "
                        ++ errStr
