module Contracts.SmokeSignal exposing (..)

import Config
import Eth.Abi.Decode as AbiDecode exposing (abiDecode, andMap, data, toElmDecoder, topic)
import Eth.Abi.Encode as AbiEncode exposing (Encoding(..), abiEncode)
import Eth.Types exposing (..)
import Eth.Utils as U
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (custom)
import TokenValue exposing (TokenValue)


type alias SmokeSignalWithMessageEvent =
    { hash : Hex
    , from : Address
    , burnAmount : TokenValue
    , message : String
    }


smokeSignalWithMessageEventFilter : Bool -> BlockId -> BlockId -> LogFilter
smokeSignalWithMessageEventFilter testMode from to =
    { fromBlock = from
    , toBlock = to
    , address = Config.smokesigContractAddress testMode
    , topics = [ Just <| U.keccak256 "SmokeSignalWithMessage(bytes32,address,uint256,string)" ]
    }


smokeSignalWithMessageDecoder : Decoder SmokeSignalWithMessageEvent
smokeSignalWithMessageDecoder =
    succeed SmokeSignalWithMessageEvent
        |> custom (topic 1 (AbiDecode.staticBytes 32))
        |> custom (topic 2 AbiDecode.address)
        |> custom
            (data 0
                AbiDecode.uint
                |> Decode.map TokenValue.tokenValue
            )
        |> custom (data 1 AbiDecode.string)


smokeSignalWithMessage : Bool -> String -> TokenValue -> Call Hex
smokeSignalWithMessage testMode message burnAmount =
    { to = Just <| Config.smokesigContractAddress testMode
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data =
        Just <|
            AbiEncode.functionCall
                "smokeSignalWithMessage(string,uint256)"
                [ AbiEncode.string message
                , abiEncodeTokenValue burnAmount
                ]
    , nonce = Nothing
    , decoder =
        toElmDecoder <| AbiDecode.staticBytes 32
    }


abiEncodeTokenValue : TokenValue -> Encoding
abiEncodeTokenValue tv =
    TokenValue.getEvmValue tv
        |> AbiEncode.uint
