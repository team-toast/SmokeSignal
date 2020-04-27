module Home.State exposing (..)

import Home.Types exposing (..)
import Post

init : ( Model, Cmd Msg )
init =
    ( { topicInput = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        TopicInputChanged newInput ->
            justModelUpdate
                { prevModel
                    | topicInput = newInput
                }

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]
