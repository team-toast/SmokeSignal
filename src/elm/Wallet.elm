module Wallet exposing (decodeConnectResponse, isActive, postResponseDecoder, userInfo)

import Chain
import Eth.Decode
import Eth.Types exposing (TxHash)
import Json.Decode as Decode exposing (Decoder, Value)
import Result.Extra
import TokenValue
import Types exposing (UserInfo, Wallet(..))


postResponseDecoder : Value -> Result Types.TxErr TxHash
postResponseDecoder =
    Decode.decodeValue
        ([ Eth.Decode.txHash
            |> Decode.map Ok
         , Decode.field "code" Decode.int
            |> Decode.map
                (\n ->
                    case n of
                        4001 ->
                            Types.UserRejected
                                |> Err

                        _ ->
                            Types.OtherErr ("Code: " ++ String.fromInt n)
                                |> Err
                )
         ]
            |> Decode.oneOf
        )
        >> Result.Extra.unpack
            (Decode.errorToString >> Types.OtherErr >> Err)
            identity


connectResponseDecoder : Decoder Types.WalletConnectResponse
connectResponseDecoder =
    [ Decode.map3 UserInfo
        (Decode.field "address" Eth.Decode.address)
        (Decode.field "balance" Eth.Decode.bigInt
            |> Decode.map TokenValue.tokenValue
        )
        (Decode.field "network" Chain.decodeChain)
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
