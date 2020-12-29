module Home.State exposing (..)

import Common.Msg
import Home.Types exposing (..)
import Post
import PostUX.State as PostUX
import UserNotice as UN


init : ( Model, Cmd Msg )
init =
    ( { maybePostUXModel = Nothing
      , topicSearchInput = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        PostUXMsg postUXMsg ->
            case prevModel.maybePostUXModel of
                Nothing ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ Common.Msg.AddUserNotice <|
                            UN.unexpectedError
                                "PostUX msg received, but there is no postUXModel!"
                                postUXMsg
                        ]

                Just postUXModel ->
                    let
                        postUXUpdateResult =
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
