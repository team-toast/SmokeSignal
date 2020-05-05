module PostUX.State exposing (..)

import Post exposing (Post)
import PostUX.Types exposing (..)


init : Model
init =
    { showAddress = False
    }


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        NoOp ->
            justModelUpdate prevModel

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]

        ShowOrHideAuthorAddress ->
            justModelUpdate
                { prevModel
                    | showAddress =
                        not prevModel.showAddress
                }
