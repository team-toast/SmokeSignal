module Home.Types exposing (..)

import Common.Msg exposing (..)


type alias Model =
    { topicInput : String
    }


type Msg
    = TopicInputChanged String
    | MsgUp MsgUp


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { newModel = model
    , cmd = Cmd.none
    , msgUps = []
    }
