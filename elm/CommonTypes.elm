module CommonTypes exposing (..)

import Dict
import Eth.Net
import Eth.Types exposing (Address, Hex)
import Json.Decode
import Json.Encode
import TokenValue exposing (TokenValue)


type DisplayProfile
    = Desktop
    | Mobile


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    , balance : Maybe TokenValue
    , daiUnlocked : Maybe Bool
    }


type alias PostId =
    { block : Int
    , messageHash : Hex
    }


withBalance : TokenValue -> UserInfo -> UserInfo
withBalance balance userInfo =
    { userInfo
        | balance = Just balance
    }


withIsUnlocked : Bool -> UserInfo -> UserInfo
withIsUnlocked unlocked userInfo =
    { userInfo
        | daiUnlocked = Just <| unlocked
    }


screenWidthToDisplayProfile : Int -> DisplayProfile
screenWidthToDisplayProfile width =
    if width >= 1150 then
        Desktop

    else
        Mobile


changeForMobile : a -> DisplayProfile -> a -> a
changeForMobile changed dProfile original =
    case dProfile of
        Desktop ->
            original

        Mobile ->
            changed

type alias EncodedMessageDraft =
    { author : Address
    , encodedMessageAndMetadata : String
    , burnAmount : TokenValue
    , donateAmount : TokenValue
    }