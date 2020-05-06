module Wallet exposing (..)

import Common.Types exposing (..)
import Config
import Eth.Net
import Eth.Types exposing (Address, HttpProvider, TxHash, WebsocketProvider)
import Helpers.Eth as EthHelpers
import TokenValue exposing (TokenValue)


type Wallet
    = NoneDetected
    | OnlyNetwork Eth.Net.NetworkId
    | Active UserInfo


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


withUnlockStatus : UnlockStatus -> Wallet -> Wallet
withUnlockStatus status wallet =
    case wallet of
        Active uInfo ->
            Active <|
                (uInfo |> Common.Types.withUnlockStatus status)

        _ ->
            wallet

unlockStatus : Wallet -> UnlockStatus
unlockStatus wallet =
    case wallet of
        Active uInfo ->
            uInfo.unlockStatus
        _ ->
            NotConnected