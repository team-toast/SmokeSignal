module PostUX.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font
import ElementMarkdown
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.List as ListHelpers
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra
import Post exposing (Post)
import Result.Extra
import String.Extra
import Theme exposing (Theme)
import TokenValue exposing (TokenValue)


type alias Model =
    { showAddress : Bool
    , showInput : ShowInputState
    }


type Msg
    = MsgUp MsgUp
    | NoOp
    | PhaceIconClicked
    | SupportBurnClicked
    | SupportTipClicked
    | AmountInputChanged String
    | SupportTipSubmitClicked Post.Id TokenValue
    | SupportBurnSubmitClicked Post.Id TokenValue
    | ResetActionForm


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


type ShowInputState
    = None
    | Burn String
    | Tip String


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        []
