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
    


withIsUnlocked : Bool -> Wallet -> Wallet
withIsUnlocked isUnlocked wallet =
    case wallet of
        Active uInfo ->
            Active <|
                (uInfo |> Common.Types.withIsUnlocked isUnlocked)
        _ ->
            wallet
    
