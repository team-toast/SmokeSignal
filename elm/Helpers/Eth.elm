module Helpers.Eth exposing (..)

import Array
import BigInt exposing (BigInt)
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, HttpProvider, Tx, TxHash, WebsocketProvider)
import Eth.Utils


addressIfNot0x0 : Address -> Maybe Address
addressIfNot0x0 addr =
    if addressIs0x0 addr then
        Nothing

    else
        Just addr


addressIs0x0 : Address -> Bool
addressIs0x0 addr =
    addr == zeroAddress


zeroAddress : Address
zeroAddress =
    Eth.Utils.unsafeToAddress "0x0000000000000000000000000000000000000000"


getLogAt : Int -> List Eth.Types.Log -> Maybe Eth.Types.Log
getLogAt index logList =
    Array.fromList logList
        |> Array.get index


updateCallValue : BigInt -> Eth.Types.Call a -> Eth.Types.Call a
updateCallValue value call =
    { call
        | value = Just value
    }


maxUintValue : BigInt
maxUintValue =
    BigInt.sub
        (BigInt.pow
            (BigInt.fromInt 2)
            (BigInt.fromInt 256)
        )
        (BigInt.fromInt 1)


etherscanTxUrl : TxHash -> String
etherscanTxUrl txHash =
    "https://etherscan.io/tx/"
        ++ Eth.Utils.txHashToString txHash
