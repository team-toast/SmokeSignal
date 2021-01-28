module View.PostPage exposing (view)

import Element exposing (Element, column, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, white)
import Theme
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, whiteGlowAttributeSmall)
import View.Common
import View.Img
import View.Markdown


view : Model -> Published -> Element Msg
view model post =
    [ [ [ text (post.core.content.title |> Maybe.withDefault ". . .") ]
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
    , post.core.content.body
        |> View.Markdown.renderString
            [ padding 10
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            , width fill
            ]
        |> Result.toMaybe
        |> View.Common.whenJust identity
    ]
        |> column
            [ height fill
            , spacing 20
            , width fill
            , padding 20
            , sansSerifFont
            ]
