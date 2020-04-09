module Message exposing (..)

import CommonTypes exposing (..)
import Contracts.SmokeSignal as SSContract
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
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
                    , Ok noMetadata
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
    { reply : Maybe PostId }


noMetadata =
    Metadata Nothing


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
                , metadata.reply
                    |> Maybe.map encodePostId
                    |> Maybe.map (Tuple.pair "re")
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


encodePostId : PostId -> E.Value
encodePostId postId =
    E.list identity
        [ E.int postId.block
        , encodeHex postId.messageHash
        ]


metadataDecoder : D.Decoder Metadata
metadataDecoder =
    D.map
        Metadata
        (D.maybe (D.field "re" postIdDecoder))


postIdDecoder : D.Decoder PostId
postIdDecoder =
    D.map2
        PostId
        (D.index 0 D.int)
        (D.index 1 hexDecoder)


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
