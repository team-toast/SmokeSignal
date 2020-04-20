module Home.State exposing (..)

import Home.Types exposing (..)


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
                { prevModel | topicInput = String.toLower newInput }

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]
