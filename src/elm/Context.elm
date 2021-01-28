module Context exposing (..)

import Eth.Types exposing (Hex)


type Context
    = Reply PostId
    | TopLevel String


type alias PostId =
    { block : Int
    , messageHash : Hex
    }
