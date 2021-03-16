port module Ports exposing (connectToWeb3, consentToCookies, log, postResponse, setDescription, setOnboarded, setVisited, submitPost, txIn, txOut, walletResponse, xDaiImport)

import Json.Decode exposing (Value)



-- OUT


port log : String -> Cmd msg


port connectToWeb3 : () -> Cmd msg


port txOut : Value -> Cmd msg


port submitPost : Value -> Cmd msg


port consentToCookies : () -> Cmd msg


port setDescription : String -> Cmd msg


port setOnboarded : () -> Cmd msg


port setVisited : () -> Cmd msg


port xDaiImport : () -> Cmd msg



-- IN


port txIn : (Value -> msg) -> Sub msg


port postResponse : (Value -> msg) -> Sub msg


port walletResponse : (Value -> msg) -> Sub msg
