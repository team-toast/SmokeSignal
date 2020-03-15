module Wallet exposing (..)

import CommonTypes exposing (..)
import Eth.Net
import Eth.Types exposing (Address, HttpProvider, TxHash, WebsocketProvider)
import Helpers.Eth as EthHelpers
import Config

type State
    = NoneDetected
    | OnlyNetwork Eth.Net.NetworkId
    | Active UserInfo


userInfo : State -> Maybe UserInfo
userInfo walletState =
    case walletState of
        Active uInfo ->
            Just uInfo

        _ ->
            Nothing


httpProvider : State -> Maybe HttpProvider
httpProvider walletState =
    network walletState
        |> Maybe.andThen EthHelpers.networkToHttpProvider


-- httpProviderWithDefault : State -> HttpProvider
-- httpProviderWithDefault walletState =
--     httpProvider walletState
--         |> Maybe.withDefault (Config.mainnetHttpProviderUrl)


network : State -> Maybe Eth.Net.NetworkId
network walletState =
    case walletState of
        NoneDetected ->
            Nothing

        OnlyNetwork network_ ->
            Just network_

        Active uInfo ->
            Just uInfo.network
