module Wallet exposing (decodeConnectResponse, network, userInfo)

import Eth.Decode
import Eth.Net
import Eth.Sentry.Wallet
import Json.Decode as Decode exposing (Decoder, Value)
import TokenValue exposing (TokenValue)
import Types exposing (UserInfo, Wallet(..))


connectResponseDecoder : Decoder Types.WalletConnectResponse
connectResponseDecoder =
    [ Decode.map2 Types.WalletInfo
        (Decode.field "walletSentry" Eth.Sentry.Wallet.decoder)
        (Decode.field "balance"
            (Eth.Decode.bigInt
                |> Decode.map TokenValue.tokenValue
            )
            |> Decode.nullable
        )
        |> Decode.map Types.WalletSucceed
    , Decode.field "code" Decode.int
        |> Decode.map
            (\n ->
                case n of
                    4001 ->
                        Types.WalletCancel

                    _ ->
                        Types.WalletError
            )
    , Decode.null Types.WalletClear
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


network : Wallet -> Maybe Eth.Net.NetworkId
network walletState =
    case walletState of
        NoneDetected ->
            Nothing

        Connecting ->
            Nothing

        NetworkReady ->
            Nothing

        Active uInfo ->
            Just uInfo.network
