module Wallet exposing (decodeConnectResponse, isActive, userInfo)

import Eth.Decode
import Eth.Net
import Json.Decode as Decode exposing (Decoder, Value)
import TokenValue
import Types exposing (UserInfo, Wallet(..))


connectResponseDecoder : Decoder Types.WalletConnectResponse
connectResponseDecoder =
    [ Decode.map3 UserInfo
        (Decode.field "address" Eth.Decode.address)
        (Decode.field "balance" Eth.Decode.bigInt
            |> Decode.map TokenValue.tokenValue
        )
        (Decode.field "network" Eth.Net.networkIdDecoder
            |> Decode.map
                (\network ->
                    case network of
                        Eth.Net.Private 100 ->
                            Types.XDai

                        _ ->
                            Types.Eth
                )
        )
        |> Decode.map Types.WalletSucceed
    , Decode.field "code" Decode.int
        |> Decode.map
            (\n ->
                case n of
                    4001 ->
                        Types.WalletCancel

                    32002 ->
                        Types.WalletInProgress

                    _ ->
                        Types.WalletError
            )
    ]
        |> Decode.oneOf


decodeConnectResponse : Value -> Types.WalletConnectResponse
decodeConnectResponse =
    Decode.decodeValue connectResponseDecoder
        >> Result.withDefault Types.WalletError


userInfo : Wallet -> Maybe UserInfo
userInfo walletState =
    case walletState of
        Active uInfo ->
            Just uInfo

        _ ->
            Nothing


isActive : Wallet -> Bool
isActive walletState =
    case walletState of
        Active _ ->
            True

        _ ->
            False
