module Contracts.SmokeSignal exposing (..)

import CommonTypes exposing (..)
import Config
import Contracts.Generated.SmokeSignal as G
import Eth.Types exposing (..)
import Eth.Utils as U
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (custom)
import TokenValue exposing (TokenValue)


type alias MessageBurn =
    { hash : Hex
    , from : Address
    , burnAmount : TokenValue
    , message : String
    }


convertBurnAmount : G.MessageBurn -> MessageBurn
convertBurnAmount gen =
    MessageBurn
        gen.hash
        gen.from
        (TokenValue.tokenValue gen.burnAmount)
        gen.message


messageBurnEventFilter : BlockId -> BlockId -> Maybe Hex -> Maybe Address -> LogFilter
messageBurnEventFilter from to maybeHash maybeAuthor =
    G.messageBurnEvent
        Config.smokesignalContractAddress
        maybeHash
        maybeAuthor
        |> (\filter ->
                { filter
                    | fromBlock = from
                    , toBlock = to
                }
           )


messageBurnDecoder : Decoder MessageBurn
messageBurnDecoder =
    G.messageBurnDecoder
        |> Decode.map convertBurnAmount


burnEncodedMessage : EncodedMessageDraft -> Call Hex
burnEncodedMessage encodedMessage =
    G.burnMessage
        Config.smokesignalContractAddress
        encodedMessage.encodedMessageAndMetadata
        (TokenValue.getEvmValue encodedMessage.burnAmount)
        (TokenValue.getEvmValue encodedMessage.donateAmount)
