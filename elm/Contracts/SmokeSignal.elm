module Contracts.SmokeSignal exposing (..)

import Config
import Contracts.Generated.SmokeSignal as G
import Eth.Types exposing (..)
import Eth.Utils as U
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (custom)
import TokenValue exposing (TokenValue)


type alias SmokeSignalWithMessage =
    { hash : Hex
    , from : Address
    , burnAmount : TokenValue
    , message : String
    }


convertBurnAmount : G.SmokeSignalWithMessage -> SmokeSignalWithMessage
convertBurnAmount gen =
    SmokeSignalWithMessage
        gen.hash
        gen.from
        (TokenValue.tokenValue gen.burnAmount)
        gen.message


smokeSignalWithMessageEventFilter : BlockId -> BlockId -> Maybe Hex -> Maybe Address -> LogFilter
smokeSignalWithMessageEventFilter from to maybeHash maybeAuthor =
    G.smokeSignalWithMessageEvent
        Config.smokesignalContractAddress
        maybeHash
        maybeAuthor
        |> (\filter ->
                { filter
                    | fromBlock = from
                    , toBlock = to
                }
           )


smokeSignalWithMessageDecoder : Decoder SmokeSignalWithMessage
smokeSignalWithMessageDecoder =
    G.smokeSignalWithMessageDecoder
        |> Decode.map convertBurnAmount


smokeSignalWithMessage : String -> TokenValue -> Call Hex
smokeSignalWithMessage message burnAmount =
    G.smokeSignalWithMessage
        Config.smokesignalContractAddress
        message
        (TokenValue.getEvmValue burnAmount)
