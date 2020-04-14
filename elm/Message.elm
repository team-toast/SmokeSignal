module Message exposing (..)

import CommonTypes exposing (..)
import Contracts.SmokeSignal as SSContract
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.List as ListHelpers
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra
import Result.Extra
import TokenValue exposing (TokenValue)


type alias Draft =
    { author : Address
    , message : String
    , burnAmount : TokenValue
    , donateAmount : TokenValue
    , metadata : Metadata
    }


type alias Message =
    { postId : PostId
    , from : Address
    , burnAmount : TokenValue
    , message : String
    , metadata : Result D.Error Metadata
    }


fromContractEvent : Int -> SSContract.MessageBurn -> Message
fromContractEvent block messageEvent =
    let
        ( extractedMessage, extractedMetadata ) =
            case ( String.left 12 messageEvent.message, String.dropLeft 12 messageEvent.message ) of
                ( "!smokesignal", jsonStr ) ->
                    case decodeMessageAndMetadata jsonStr of
                        Ok ( message, metadata ) ->
                            ( message, Ok metadata )

                        Err errStr ->
                            ( messageEvent.message, Err errStr )

                _ ->
                    ( messageEvent.message
                    , Ok nullMetadata
                    )
    in
    Message
        (PostId
            block
            messageEvent.hash
        )
        messageEvent.from
        messageEvent.burnAmount
        extractedMessage
        extractedMetadata


type alias Metadata =
    { metadataVersion : Int
    , replyTo : Maybe PostId
    , topic : Maybe String
    }

nullMetadata =
    Metadata
        0
        Nothing
        Nothing


getTopic : Message -> Maybe String
getTopic message =
    message.metadata
        |> Result.toMaybe
        |> Maybe.andThen .topic


currentMetadataVersion =
    2


versionedMetadata : Maybe PostId -> Maybe String -> Metadata
versionedMetadata =
    Metadata currentMetadataVersion


blankVersionedMetadata : Metadata
blankVersionedMetadata =
    versionedMetadata Nothing Nothing


encodeDraft : Draft -> EncodedMessageDraft
encodeDraft draft =
    EncodedMessageDraft
        draft.author
        ("!smokesignal" ++ encodeMessageAndMetadataToString ( draft.message, draft.metadata ))
        draft.burnAmount
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
                , Maybe.map (Tuple.pair "topic") <|
                    Maybe.map E.string metadata.topic
                ]
        )


decodeMessageAndMetadata : String -> Result D.Error ( String, Metadata )
decodeMessageAndMetadata =
    D.decodeString messageDecoder


messageDecoder : D.Decoder ( String, Metadata )
messageDecoder =
    D.map2
        Tuple.pair
        (D.field "m" D.string)
        metadataDecoder


metadataDecoder : D.Decoder Metadata
metadataDecoder =
    D.map3
        Metadata
        (D.maybe
            (D.field "v" D.int)
            |> D.map (Maybe.withDefault 1)
        )
        (D.maybe (D.field "re" postIdDecoder))
        (D.maybe (D.field "topic" D.string))


encodePostId : PostId -> E.Value
encodePostId postId =
    E.list identity
        [ E.int postId.block
        , encodeHex postId.messageHash
        ]


postIdDecoder : D.Decoder PostId
postIdDecoder =
    D.map2
        PostId
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
