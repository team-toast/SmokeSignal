port module Ports exposing (burnOrTipResponse, chainSwitchResponse, connectToWeb3, consentToCookies, log, postResponse, setDescription, setOnboarded, setVisited, submitPost, submitBurnOrTip, walletResponse, xDaiImport, setGtagUrlPath)

import Json.Decode exposing (Value)



-- OUT


port log : String -> Cmd msg


port connectToWeb3 : () -> Cmd msg


port submitBurnOrTip : Value -> Cmd msg


port submitPost : Value -> Cmd msg


port consentToCookies : () -> Cmd msg


port setDescription : String -> Cmd msg


port setGtagUrlPath : String -> Cmd msg


port setOnboarded : () -> Cmd msg


port setVisited : () -> Cmd msg


port xDaiImport : () -> Cmd msg



-- IN


port burnOrTipResponse : (Value -> msg) -> Sub msg


port postResponse : (Value -> msg) -> Sub msg


port chainSwitchResponse : (Value -> msg) -> Sub msg


port walletResponse : (Value -> msg) -> Sub msg
