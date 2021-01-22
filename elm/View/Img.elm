module View.Img exposing (bookmark, hide, replyArrow, speechBubble)

import Element exposing (Color, Element)
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeLinejoin, strokeWidth, viewBox, width)


speechBubble : Int -> Color -> Element msg
speechBubble size color =
    svg
        [ viewBox "0 0 66.32 43.94"
        , height <| String.fromInt size
        , width <| String.fromInt size
        ]
        [ Svg.path
            [ fill <| rgb color
            , d "M55.54 0H10.78A10.78 10.78 0 000 10.78V25.7a10.78 10.78 0 0010.78 10.78h2.64l2.33 7.46 3.73-7.46h36.06A10.78 10.78 0 0066.32 25.7V10.78A10.78 10.78 0 0055.54 0z"
            ]
            []
        ]
        |> wrap


hide : Int -> Color -> Element msg
hide size color =
    svg
        [ viewBox "0 0 30.74 30.73"
        , height <| String.fromInt size
        , width <| String.fromInt size
        ]
        [ Svg.path
            [ fill <| rgb color
            , d "M15.37 0a15.37 15.37 0 1015.37 15.37A15.38 15.38 0 0015.37 0zm11.55 15.37a11.53 11.53 0 01-2 6.45l-16-16a11.54 11.54 0 0118 9.58zm-23.11 0a11.37 11.37 0 012.37-6.92l16.11 16.11a11.42 11.42 0 01-6.92 2.36A11.57 11.57 0 013.81 15.37z"
            ]
            []
        ]
        |> wrap


replyArrow : Int -> Color -> Element msg
replyArrow size color =
    svg
        [ viewBox "0 0 73 63"
        , height <| String.fromInt size
        , width <| String.fromInt size
        ]
        [ g
            [ fill "none"
            , stroke <| rgb color
            , strokeWidth "6.2"
            ]
            [ path [ strokeLinejoin "round", d "M70 0v23c0 16-12 22-23 23H5" ] []
            , path [ d "M2 48l18-19M2 44l19 17" ] []
            ]
        ]
        |> wrap


bookmark : Int -> Color -> Element msg
bookmark size color =
    Svg.svg
        [ viewBox "0 0 35 44"
        , height <| String.fromInt size
        , width <| String.fromInt size
        ]
        [ path
            [ fill <| rgb color
            , d "M35 44L22 34 10 44V16H0V6a6 6 0 015-6 7 7 0 011 0h22a6 6 0 017 6 7 7 0 010 1v37zM10 13V3H6a3 3 0 00-1 1 3 3 0 00-2 2v7z"
            ]
            []
        ]
        |> wrap


{-| Html needs to be wrapped in an 'el' in order to
be positioned correctly by elm-ui.
-}
wrap : Svg msg -> Element msg
wrap =
    Element.html
        >> Element.el []


rgb : Color -> String
rgb =
    Element.toRgb
        >> (\{ red, green, blue } ->
                [ red, green, blue ]
           )
        >> List.map ((*) 255 >> round >> String.fromInt)
        >> String.join ", "
        >> (\str -> "rgb(" ++ str ++ ")")
