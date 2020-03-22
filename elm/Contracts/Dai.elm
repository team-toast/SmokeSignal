module Contracts.Dai exposing (..)

import Config
import Contracts.ERC20Generated as ERC20
import Eth
import Eth.Types exposing (..)
import Helpers.Eth as EthHelpers
import Http
import Task
import TokenValue exposing (TokenValue)


getBalanceCmd : Address -> Bool -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getBalanceCmd owner testMode msgConstructor =
    Eth.call
        (Config.httpProviderUrl testMode)
        (ERC20.balanceOf
            (Config.daiContractAddress testMode)
            owner
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getAllowanceCmd : Address -> Bool -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getAllowanceCmd owner testMode msgConstructor =
    Eth.call
        (Config.httpProviderUrl testMode)
        (ERC20.allowance
            (Config.daiContractAddress testMode)
            owner
            (Config.smokesigContractAddress testMode)
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


unlockDaiCall : Bool -> Call Bool
unlockDaiCall testMode =
    ERC20.approve
        (Config.daiContractAddress testMode)
        (Config.smokesigContractAddress testMode)
        EthHelpers.maxUintValue
