module View.Topic exposing (view)

import Dict
import Element exposing (Element, centerX, column, el, fill, height, padding, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (black, white)
import Set
import Theme exposing (orange)
import Types exposing (..)
import View.Attrs exposing (hover, slightRound, whiteGlowAttributeSmall)
import View.Img
import View.Post


view : Model -> String -> Element Msg
view model topic =
    let
        posts =
            model.rootPosts
                |> Dict.values
                |> List.filter
                    (.topic
                        >> String.toLower
                        >> (==) (String.toLower topic)
                    )
    in
    [ topicHeader topic
    , if List.isEmpty posts then
        text "Be the first to create a post on this topic."
            |> el
                [ padding 10
                , whiteGlowAttributeSmall
                , Background.color black
                , Font.color white
                , centerX
                ]

      else
        posts
            |> List.map
                (\post ->
                    View.Post.view
                        model.dProfile
                        (model.blockTimes
                            |> Dict.get post.core.id.block
                        )
                        model.now
                        (model.replyIds
                            |> Dict.get post.core.key
                            |> Maybe.withDefault Set.empty
                        )
                        (model.accounting
                            |> Dict.get post.core.key
                        )
                        (model.tipOpen
                            |> Maybe.andThen
                                (\x ->
                                    if x.id == post.core.id then
                                        Just x.showInput

                                    else
                                        Nothing
                                )
                        )
                        model.ethPrice
                        model.compose.dai
                        topic
                        post.core
                )
            |> column
                [ width fill
                , spacing 10
                ]
    ]
        |> column
            [ width fill
            , height fill
            , spacing 10
            , Element.alignTop
            ]


topicHeader : String -> Element Msg
topicHeader topic =
    [ [ topic
            |> text
            |> el [ Font.size 35 ]
      , View.Img.bookmark 30 orange
      ]
        |> row
            [ width fill
            , spaceEvenly
            , Background.color black
            , Font.color white
            , padding 15
            ]
    , Input.button
        [ View.Attrs.sansSerifFont
        , padding 10
        , slightRound
        , Background.color Theme.orange
        , Font.bold
        , Font.color white
        , Font.size 20
        , width fill
        , hover
        ]
        { onPress = Just Types.ComposeToggle
        , label = text "Comment..."
        }
    ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            ]
