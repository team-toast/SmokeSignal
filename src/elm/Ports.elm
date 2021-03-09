port module Ports exposing (connectToWeb3, consentToCookies, log, postResponse, setDescription, setVisited, submitPost, txIn, txInX, txOut, txOutX, walletResponse, xDaiImport)

import Json.Decode exposing (Value)



-- OUT


port log : String -> Cmd msg


port connectToWeb3 : () -> Cmd msg


port txOut : Value -> Cmd msg


port txOutX : Value -> Cmd msg


port submitPost : Value -> Cmd msg


port consentToCookies : () -> Cmd msg


port setDescription : String -> Cmd msg


port setVisited : () -> Cmd msg


port xDaiImport : () -> Cmd msg



-- IN


port txIn : (Value -> msg) -> Sub msg


port txInX : (Value -> msg) -> Sub msg


port postResponse : (Value -> msg) -> Sub msg


port walletResponse : (Value -> msg) -> Sub msg
