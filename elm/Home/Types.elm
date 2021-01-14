module Home.Types exposing (..)

import Common.Types exposing (..)
import PostUX.Types as PostUX


type alias Model =
    { maybePostUXModel : Maybe PostUX.Model
    , topicSearchInput : String
    , showNewToSmokeSignalModal : Bool
    }


type Msg
    = PostUXMsg PostUX.Msg
    | MsgUp MsgUp
    | SearchInputChanged String
    | CloseNewToSmokeSignalModal
    | ShowNewToSmokeSignalModal


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


justModelUpdate :
    Model
    -> UpdateResult
justModelUpdate model =
    { newModel = model
    , cmd = Cmd.none
    , msgUps = []
    }
