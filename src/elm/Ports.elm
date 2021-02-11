port module Ports exposing (connectToWeb3, consentToCookies, log, setDescription, setVisited, txIn, txOut, walletSentryPort)

import Json.Decode exposing (Value)



-- OUT


port log : String -> Cmd msg


port connectToWeb3 : () -> Cmd msg


port txOut : Value -> Cmd msg


port consentToCookies : () -> Cmd msg


port setDescription : String -> Cmd msg


port setVisited : () -> Cmd msg



-- IN


port walletSentryPort : (Value -> msg) -> Sub msg


port txIn : (Value -> msg) -> Sub msg
