module View.Topics exposing (view)

import Dict exposing (Dict)
import Element exposing (Element, column, el, fill, height, padding, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (black, white)
import Misc
import Set
import Theme exposing (orange)
import Types exposing (..)
import View.Attrs exposing (hover, sansSerifFont, whiteGlowAttributeSmall)
import View.Common exposing (whenAttr)
import View.Img


view : Model -> Element Msg
view model =
    [ viewTopicSearch model
    , model.topics
        |> Misc.sortTopics
        |> viewTopics
    ]
        |> column
            [ width fill
            , height fill
            , spacing 10
            , Element.alignTop
            ]


viewTopicSearch : Model -> Element Msg
viewTopicSearch model =
    let
        currentTopic =
            model.topicInput
                |> Misc.validateTopic

        dropdown =
            currentTopic
                |> View.Common.whenJust (viewDropdown model.topics)
    in
    [ "Topics"
        |> text
        |> el [ Font.size 35, Font.color black ]
        |> el
            [ width fill
            , Background.color orange
            , Font.color white
            , padding 15
            ]
    , [ Input.text
            [ width fill
            , Background.color black
            , Border.color Theme.almostWhite
            , Font.color white
            , View.Attrs.sansSerifFont
            , Input.button
                [ Element.alignRight
                , hover
                , Element.centerY
                , Element.paddingXY 5 0
                ]
                { onPress = Just <| TopicInputChange ""
                , label = View.Img.close 30 white
                }
                |> Element.inFront
                |> whenAttr (not <| String.isEmpty model.topicInput)
            , Border.width 1
            , dropdown
                |> Element.below
            ]
            { onChange = TopicInputChange
            , text = model.topicInput
            , placeholder =
                Element.text "Find topic"
                    |> Input.placeholder
                        [ Font.color white
                        ]
                    |> Just
            , label = Input.labelHidden "topic"
            }

      --, Input.button
      --[ Background.color Theme.orange
      --, padding 10
      --, View.Attrs.roundBorder
      --, hover
      --, Element.alignRight
      --, View.Attrs.sansSerifFont
      --]
      --{ onPress = Just TopicSubmit
      --, label = text "Submit"
      --}
      ]
        |> column [ padding 20, width fill, spacing 10 ]
    ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            , Background.color black
            ]


viewDropdown : Dict String Count -> String -> Element Msg
viewDropdown topics topic =
    let
        ts =
            topics
                |> Dict.keys
                |> List.filter (String.startsWith topic)
    in
    (if List.isEmpty ts then
        Input.button
            [ Background.color Theme.orange
            , padding 10
            , View.Attrs.roundBorder
            , hover
            , Element.alignRight
            , View.Attrs.sansSerifFont
            , Font.color black
            ]
            { onPress = Just TopicSubmit
            , label = text "Use this topic"
            }

     else
        ts
            |> List.map
                (\t ->
                    Input.button
                        [ width fill
                        , Border.color Theme.almostWhite
                        , Border.width 1
                        , padding 10
                        , hover
                        , Font.color black
                        ]
                        { onPress = Just <| GotoView <| ViewTopic t
                        , label = text t
                        }
                )
            |> column [ width fill ]
    )
        |> el
            [ width fill
            , padding 5
            , Background.color Theme.almostWhite
            ]


viewTopics : List ( String, Count ) -> Element Msg
viewTopics =
    List.map
        (\( topic, count ) ->
            Input.button
                [ width fill
                , Background.color black
                , Font.color white
                , padding 20
                , hover
                , whiteGlowAttributeSmall
                ]
                { onPress = Just <| GotoView <| ViewTopic topic
                , label =
                    [ [ [ text topic ]
                            |> paragraph [ width fill, Font.size 40 ]
                      , [ View.Img.dollar 25 white
                        , count.total
                            |> Misc.formatDollar
                            |> text
                            |> el [ Font.size 30, Font.bold ]
                        ]
                            |> row [ View.Attrs.sansSerifFont ]
                      ]
                        |> row
                            [ width fill
                            ]
                    , count.ids
                        |> Set.size
                        |> (\len ->
                                let
                                    suffix =
                                        if len == 1 then
                                            ""

                                        else
                                            "s"
                                in
                                String.fromInt len ++ " post" ++ suffix
                           )
                        |> text
                        |> el [ Element.alignRight, sansSerifFont ]
                    ]
                        |> column
                            [ width fill
                            , spacing 10
                            ]
                }
        )
        >> column [ width fill, spacing 10 ]
