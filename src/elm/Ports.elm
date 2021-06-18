port module Ports exposing (burnOrTipResponse, chainSwitchResponse, connectToWalletConnect, connectToWeb3, consentToCookies, fbEvent, log, onUrlChange, postResponse, pushUrl, setDescription, setTitle, setVisited, share, submitBurnOrTip, submitPost, submitTemp, walletConnectResponse, walletResponse, xDaiImport)

import Json.Decode exposing (Value)



-- OUT


port log : String -> Cmd msg


port connectToWeb3 : () -> Cmd msg


port connectToWalletConnect : () -> Cmd msg


port submitTemp : Value -> Cmd msg


port submitBurnOrTip : { provider : String, params : Value } -> Cmd msg


port submitPost : { provider : String, params : Value } -> Cmd msg


port consentToCookies : () -> Cmd msg


port setDescription : String -> Cmd msg


port setVisited : () -> Cmd msg


port xDaiImport : () -> Cmd msg


port share : Value -> Cmd msg


port pushUrl : String -> Cmd msg


port setTitle : String -> Cmd msg


port fbEvent : { tag : String, name : String, data : Maybe Value } -> Cmd msg



-- IN


port burnOrTipResponse : (Value -> msg) -> Sub msg


port postResponse : (Value -> msg) -> Sub msg


port chainSwitchResponse : (Value -> msg) -> Sub msg


port walletResponse : (Value -> msg) -> Sub msg


port walletConnectResponse : (Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg
