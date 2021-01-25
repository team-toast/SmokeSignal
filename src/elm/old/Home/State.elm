module Home.State exposing (..)

import Home.Types exposing (..)
import Post
import PostUX.State as PostUX
import PostUX.Types
import UserNotice as UN


init : ( Model, Cmd Msg )
init =
    ( { maybePostUXModel = Nothing
      , topicSearchInput = ""
      , showNewToSmokeSignalModal = False
      }
    , Cmd.none
    )


update :
    Msg
    -> Model
    -> UpdateResult
update msg prevModel =
    case msg of
        PostUXMsg postUXMsg ->
            let
                postUXUpdateResult =
                    case prevModel.maybePostUXModel of
                        Nothing ->
                            PostUX.Types.UpdateResult
                                PostUX.init
                                Cmd.none
                                []

                        Just postUXModel ->
                            PostUX.update postUXMsg postUXModel
            in
            UpdateResult
                { prevModel
                    | maybePostUXModel = Just <| postUXUpdateResult.newModel
                }
                (Cmd.map PostUXMsg postUXUpdateResult.cmd)
                postUXUpdateResult.msgUps

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]

        SearchInputChanged text ->
            UpdateResult
                { prevModel
                    | topicSearchInput = text
                }
                Cmd.none
                []

        CloseNewToSmokeSignalModal ->
            UpdateResult
                { prevModel
                    | showNewToSmokeSignalModal = False
                }
                Cmd.none
                []

        ShowNewToSmokeSignalModal ->
            UpdateResult
                { prevModel
                    | showNewToSmokeSignalModal = True
                }
                Cmd.none
                []
