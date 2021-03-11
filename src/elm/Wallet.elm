module Wallet exposing (connectResponseDecoder, isActive, postResponseDecoder, userInfo)

import Chain
import Eth.Decode
import Eth.Types exposing (TxHash)
import Json.Decode as Decode exposing (Value)
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


connectResponseDecoder : Value -> Result Types.WalletConnectErr UserInfo
connectResponseDecoder =
    Decode.decodeValue
        ([ Decode.field "network" Chain.decodeChain
            |> Decode.andThen
                (\networkRes ->
                    Decode.map2
                        (\addr bal ->
                            networkRes
                                |> Result.map
                                    (\chain ->
                                        { address = addr
                                        , balance = bal
                                        , chain = chain
                                        }
                                    )
                        )
                        (Decode.field "address" Eth.Decode.address)
                        (Decode.field "balance" Eth.Decode.bigInt
                            |> Decode.map TokenValue.tokenValue
                        )
                )
         , Decode.field "code" Decode.int
            |> Decode.map
                (\n ->
                    case n of
                        4001 ->
                            Types.WalletCancel
                                |> Err

                        32002 ->
                            Types.WalletInProgress
                                |> Err

                        _ ->
                            Types.WalletError ("Code: " ++ String.fromInt n)
                                |> Err
                )
         ]
            |> Decode.oneOf
        )
        >> Result.Extra.unpack
            (Decode.errorToString >> Types.WalletError >> Err)
            identity


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
