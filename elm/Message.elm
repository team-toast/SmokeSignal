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
    { transactionHash : TxHash
    , messageHash : Hex
    , from : Address
    , burnAmount : TokenValue
    , message : String
    , metadata : Result D.Error Metadata
    }


fromContractEvent : TxHash -> SSContract.MessageBurn -> Message
fromContractEvent txHash messageEvent =
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
        txHash
        messageEvent.hash
        messageEvent.from
        messageEvent.burnAmount
        extractedMessage
        extractedMetadata


type alias Metadata =
    { reply : Maybe Hex }


noMetadata =
    Metadata Nothing


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
    D.map
        Metadata
        (D.maybe (D.field "re" hexDecoder))


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


encodeDraft : Draft -> EncodedMessageDraft
encodeDraft draft =
    EncodedMessageDraft
        draft.author
        ("!smokesignal" ++ (encodeMessageAndMetadataToString ( draft.message, draft.metadata )))
        draft.burnAmount
        draft.donateAmount


encodeMessageAndMetadataToString : ( String, Metadata ) -> String
encodeMessageAndMetadataToString ( message, metadata ) =
    E.encode 0
        (E.object <|
            Maybe.Extra.values <|
                [ Just ( "m", E.string message )
                , metadata.reply
                    |> Maybe.map encodeHex
                    |> Maybe.map (Tuple.pair "re")
                ]
        )


encodeHex : Hex -> E.Value
encodeHex =
    Eth.Utils.hexToString >> E.string
