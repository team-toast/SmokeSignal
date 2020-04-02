module CommonTypes exposing (..)

import Dict
import Eth.Net
import Eth.Types exposing (Address)
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


withBalance : TokenValue -> UserInfo -> UserInfo
withBalance balance userInfo =
    { userInfo
        | balance = Just balance
    }


withAllowance : TokenValue -> UserInfo -> UserInfo
withAllowance allowance userInfo =
    { userInfo
        | daiUnlocked = Just <| TokenValue.isMaxTokenValue allowance
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
