module View.Img exposing (bookmark)

import Element exposing (Color, Element)
import Svg exposing (Svg)
import Svg.Attributes exposing (d, height, viewBox, width)


bookmark : Int -> Color -> Element msg
bookmark size color =
    Svg.svg
        [ viewBox "0 0 35 44"
        , height <| String.fromInt size
        , width <| String.fromInt size
        ]
        [ Svg.path
            [ fill color
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


fill : Color -> Svg.Attribute msg
fill =
    Element.toRgb
        >> (\{ red, green, blue } ->
                [ red, green, blue ]
           )
        >> List.map ((*) 255 >> round >> String.fromInt)
        >> String.join ", "
        >> (\str -> "rgb(" ++ str ++ ")")
        >> Svg.Attributes.fill
