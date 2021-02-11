module View.Img exposing (bookmark, dollar, hide, replyArrow, speechBubble)

import Element exposing (Color, Element)
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeLinejoin, strokeWidth, viewBox, width)


dollar : Int -> Color -> Element msg
dollar size color =
    svg
        [ viewBox "0 0 67.2 97.1"
        , height <| String.fromInt size
        , width <| String.fromInt size
        ]
        [ Svg.path
            [ fill <| rgb color
            , d "M31.1 80.6h-.6A20.9 20.9 0 0116.8 76q-5.6-4.6-5.6-10.9a9.2 9.2 0 012-6.1 6.4 6.4 0 015.1-2.3 5.5 5.5 0 014 1.7 5.7 5.7 0 011.8 4.2 6.2 6.2 0 01-2.7 5.1c-1.7 1.4-2.6 2.2-2.6 2.6q0 2.1 3.7 4a18 18 0 008.6 2V48.7q-9.3-2.1-13.7-6.7T13 29.6a16.1 16.1 0 014.9-12.2q5-4.7 13.2-5.4V6.3H36v5.6c5.2.4 9.4 1.9 12.6 4.4s4.8 5.7 4.8 9.3a7.2 7.2 0 01-1.7 5 6 6 0 01-4.7 1.9 5.6 5.6 0 01-4.1-1.6 5.7 5.7 0 01-1.6-4.1 4.8 4.8 0 012.1-4.2c1.4-1 2.2-1.7 2.2-2 0-1-1-2-3-3A14.9 14.9 0 0036 16v24q10.8 2.8 15.4 7.5T56 60a19.3 19.3 0 01-5.4 14q-5.4 5.6-14.7 6.6V93H31zm0-42.1V16a13.7 13.7 0 00-8.7 3.4 10.1 10.1 0 00-3.2 7.8 9.5 9.5 0 003 7.2c2 1.8 5 3.2 9 4zM36 76.3a14.8 14.8 0 009.5-4.2 12.6 12.6 0 003.5-9.2q0-5.3-2.7-8T36 50.1z"
            ]
            []
        ]
        |> wrap


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
