port module Ports exposing (connectToWeb3, consentToCookies, gTagOut, setDescription, txIn, txOut, walletSentryPort)

import Json.Decode exposing (Value)


port walletSentryPort : (Value -> msg) -> Sub msg


port connectToWeb3 : () -> Cmd msg


port txOut : Value -> Cmd msg


port txIn : (Value -> msg) -> Sub msg


port gTagOut : Value -> Cmd msg


port consentToCookies : () -> Cmd msg


port setDescription : String -> Cmd msg
