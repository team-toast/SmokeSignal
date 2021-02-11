module View.Topics exposing (view)

import Element exposing (Element, column, el, fill, height, padding, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (black, white)
import Misc
import Theme exposing (orange)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, whiteGlowAttributeSmall)
import View.Img


view : Model -> Element Msg
view model =
    [ [ [ "Topics"
            |> text
            |> el [ Font.size 35, Font.color black ]
        , View.Img.bookmark 30 black
        ]
            |> row
                [ width fill
                , spaceEvenly
                , Background.color orange
                , Font.color white
                , padding 15
                ]
      , [ Input.text
            [ width fill
            , Background.color black
            , Border.color Theme.almostWhite
            , whiteGlowAttributeSmall
            , Font.color white
            , View.Attrs.onKeydown [ View.Attrs.onEnter ComposeOpen ]
            ]
            { onChange = TopicInputChange
            , text = model.topicInput
            , placeholder =
                Just <|
                    Input.placeholder
                        [ Font.color white
                        , Font.italic
                        ]
                        (Element.text "Create topic...")
            , label = Input.labelHidden "topic"
            }
        , Input.button
            [ Background.color Theme.orange
            , padding 10
            , View.Attrs.roundBorder
            , hover
            , Element.alignRight
            ]
            { onPress = Just ComposeOpen
            , label = text "Compose"
            }
        ]
            |> column [ padding 20, width fill, spacing 10 ]
      ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            , Background.color black
            ]
    , model.topics
        |> Misc.sortTopics
        |> viewTopics model.ethPrice
    ]
        |> column
            [ width fill
            , height fill
            , spacing 10
            , Element.alignTop
            ]


viewTopics : Float -> List ( String, TokenValue ) -> Element Msg
viewTopics ethPrice =
    List.map
        (\( topic, totalBurned ) ->
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
                    [ topic
                        |> text
                        |> el [ width fill, Font.size 40 ]
                    , [ View.Img.dollar 25 white
                      , totalBurned
                            |> Misc.tokenToDollar ethPrice
                            |> text
                            |> el [ Font.size 30, Font.bold ]
                      ]
                        |> row [ View.Attrs.sansSerifFont ]
                    ]
                        |> row
                            [ width fill
                            ]
                }
        )
        >> column [ width fill, spacing 10 ]
