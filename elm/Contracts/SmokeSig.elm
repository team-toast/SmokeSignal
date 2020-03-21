module Contracts.SmokeSig exposing (..)

import Abi.Decode as AbiDecode exposing (abiDecode, andMap, data, toElmDecoder, topic)
import Abi.Encode as AbiEncode exposing (Encoding(..), abiEncode)
import Eth.Types exposing (..)
import Eth.Utils as U
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (custom)
import TokenValue exposing (TokenValue)



type alias SmokeSignalWithMessage =
    { hash : String
    , from : Address
    , burnAmount : TokenValue
    , message : String
    }


smokeSignalWithMessageEvent : Address -> LogFilter
smokeSignalWithMessageEvent contractAddress =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| U.keccak256 "SmokeSignalWithMessage(string)" ]
    }


smokeSignalWithMessageDecoder : Decoder SmokeSignalWithMessage
smokeSignalWithMessageDecoder =
    succeed SmokeSignalWithMessage
        |> custom (topic 1 (AbiDecode.staticBytes 32))
        |> custom (topic 2 AbiDecode.address)
        |> custom
            (data 0
                AbiDecode.uint
                    |> Decode.map TokenValue.tokenValue
            )
        |> custom (data 1 AbiDecode.string)