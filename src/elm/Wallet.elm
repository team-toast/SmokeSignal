module Wallet exposing (network, userInfo, withFetchedBalance)

import Eth.Net
import Misc exposing (withBalance)
import TokenValue exposing (TokenValue)
import Types exposing (UserInfo, Wallet(..))


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

        OnlyNetwork network_ ->
            Just network_

        Active uInfo ->
            Just uInfo.network


withFetchedBalance : TokenValue -> Wallet -> Wallet
withFetchedBalance balance wallet =
    case wallet of
        Active uInfo ->
            Active <|
                (uInfo |> withBalance balance)

        _ ->
            wallet
