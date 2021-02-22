module Wallet exposing (decodeConnectResponse, infoRequest, isActive, userInfo)

import Eth
import Eth.Decode
import Eth.Net
import Eth.Types
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Task exposing (Task)
import TokenValue
import Types exposing (UserInfo, Wallet(..))


infoRequest : String -> Eth.Types.Address -> Task Http.Error UserInfo
infoRequest url address =
    Task.map2
        (\network balance ->
            { network = network
            , address = address
            , balance = TokenValue.tokenValue balance
            }
        )
        (Eth.Net.version url)
        (Eth.getBalance url address)


connectResponseDecoder : Decoder Types.WalletConnectResponse
connectResponseDecoder =
    [ Decode.list Eth.Decode.address
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


isActive : Wallet -> Bool
isActive walletState =
    case walletState of
        Active _ ->
            True

        _ ->
            False
