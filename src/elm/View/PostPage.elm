module View.PostPage exposing (view)

import Dict
import Element exposing (Element, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, white)
import Maybe.Extra exposing (unwrap)
import Set
import Theme
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, whiteGlowAttributeSmall)
import View.Common
import View.Img
import View.Markdown
import View.Post


view : Model -> CoreData -> Element Msg
view model post =
    [ [ [ text (post.content.title |> Maybe.withDefault ". . .") ]
            |> Element.paragraph
                [ Font.size 50
                , whiteGlowAttributeSmall
                , padding 10
                , Background.color black
                , Font.color white
                ]
      , Input.button [ Background.color Theme.orange, padding 20, roundBorder, hover ]
            { onPress = Just <| GotoView ViewHome
            , label =
                [ View.Img.replyArrow 20 black
                , text "Go back"
                ]
                    |> row [ spacing 10 ]
            }
      ]
        |> row
            [ spacing 20
            , width fill
            ]
    , post.content.body
        |> View.Markdown.renderString
            [ padding 10
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            , width fill
            ]
        |> Result.toMaybe
        |> View.Common.whenJust identity
    , model.replyIds
        |> Dict.get post.key
        |> unwrap [] Set.toList
        |> List.filterMap
            (\id ->
                Dict.get id model.replyPosts
            )
        |> List.map
            (\reply ->
                View.Post.view
                    model.dProfile
                    (model.blockTimes
                        |> Dict.get reply.core.id.block
                    )
                    model.now
                    (model.replyIds
                        |> Dict.get reply.core.key
                        |> Maybe.withDefault Set.empty
                    )
                    (model.accounting
                        |> Dict.get reply.core.key
                    )
                    (model.tipOpen
                        |> Maybe.andThen
                            (\x ->
                                if x.id == reply.core.id then
                                    Just x.showInput

                                else
                                    Nothing
                            )
                    )
                    model.ethPrice
                    model.compose.dai
                    ". . ."
                    reply.core
            )
        |> column
            [ width fill
            , spacing 10
            , Element.paddingEach
                { left = 50
                , right = 0
                , top = 0
                , bottom = 0
                }
            ]
    ]
        |> column
            [ height fill
            , spacing 20
            , width fill
            , padding 20
            , sansSerifFont
            , Input.button
                [ Background.color Theme.orange
                , padding 20
                , roundBorder
                , hover
                ]
                { onPress = Just ComposeToggle
                , label =
                    [ View.Img.replyArrow 20 black
                    , text "Reply"
                    ]
                        |> row [ spacing 10 ]
                }
                |> el
                    [ padding 20
                    , Element.alignRight
                    , Element.alignBottom
                    ]
                |> Element.inFront
            ]
