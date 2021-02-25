module Wallet exposing (chainDecoder, decodeConnectResponse, isActive, userInfo)

import Eth.Decode
import Eth.Net
import Json.Decode as Decode exposing (Decoder, Value)
import TokenValue
import Types exposing (Flags, UserInfo, Wallet(..))


chainDecoder : Flags -> Decoder (List Types.ChainConfig)
chainDecoder flags =
    Decode.map3
        (\chain contract scan ->
            { chain = chain
            , contract = contract
            , startScanBlock = scan
            , providerUrl =
                case chain of
                    Types.Eth ->
                        flags.ethProviderUrl

                    Types.XDai ->
                        flags.xDaiProviderUrl
            }
        )
        (Decode.field "network" decodeChain)
        (Decode.field "contract" Eth.Decode.address)
        (Decode.field "scan" Decode.int)
        |> Decode.list


decodeChain : Decoder Types.Chain
decodeChain =
    Eth.Net.networkIdDecoder
        |> Decode.andThen
            (\network ->
                case network of
                    Eth.Net.Mainnet ->
                        Types.Eth
                            |> Decode.succeed

                    Eth.Net.Private 100 ->
                        Types.XDai
                            |> Decode.succeed

                    -- Hardhat server
                    Eth.Net.Private 31337 ->
                        Types.Eth
                            |> Decode.succeed

                    _ ->
                        Decode.fail "bad network"
            )


connectResponseDecoder : Decoder Types.WalletConnectResponse
connectResponseDecoder =
    [ Decode.map3 UserInfo
        (Decode.field "address" Eth.Decode.address)
        (Decode.field "balance" Eth.Decode.bigInt
            |> Decode.map TokenValue.tokenValue
        )
        (Decode.field "network" decodeChain)
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
