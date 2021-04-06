port module Ports exposing (balanceResponse, burnOrTipResponse, chainSwitchResponse, connectToWeb3, consentToCookies, log, postResponse, refreshWallet, setDescription, setGtagUrlPath, setVisited, share, submitBurnOrTip, submitPost, walletResponse, xDaiImport)

import Json.Decode exposing (Value)



-- OUT


port log : String -> Cmd msg


port connectToWeb3 : () -> Cmd msg


port submitBurnOrTip : Value -> Cmd msg


port submitPost : Value -> Cmd msg


port consentToCookies : () -> Cmd msg


port setDescription : String -> Cmd msg


port setGtagUrlPath : String -> Cmd msg


port setVisited : () -> Cmd msg


port xDaiImport : () -> Cmd msg


port refreshWallet : String -> Cmd msg


port share : Value -> Cmd msg



-- IN


port burnOrTipResponse : (Value -> msg) -> Sub msg


port postResponse : (Value -> msg) -> Sub msg


port chainSwitchResponse : (Value -> msg) -> Sub msg


port walletResponse : (Value -> msg) -> Sub msg


port balanceResponse : (Value -> msg) -> Sub msg
