module View.Img exposing (bookmark, close, dollar, eth, globe, hide, link, logo, replyArrow, speechBubble, spinner, xDai)

import Element exposing (Color, Element)
import Svg exposing (Svg, g, path, polygon, svg)
import Svg.Attributes exposing (d, fill, points, stroke, strokeLinejoin, strokeWidth, viewBox)


globe : Int -> Color -> Element msg
globe size color =
    svg
        [ viewBox "0 0 24 24"
        , height size
        , width size
        ]
        [ Svg.path [ fill "none", d "M0 0h24v24H0V0z" ] []
        , Svg.path
            [ fill <| rgb color
            , d "M11.99 2C6.47 2 2 6.48 2 12s4.47 10 9.99 10C17.52 22 22 17.52 22 12S17.52 2 11.99 2zm6.93 6h-2.95c-.32-1.25-.78-2.45-1.38-3.56 1.84.63 3.37 1.91 4.33 3.56zM12 4.04c.83 1.2 1.48 2.53 1.91 3.96h-3.82c.43-1.43 1.08-2.76 1.91-3.96zM4.26 14C4.1 13.36 4 12.69 4 12s.1-1.36.26-2h3.38c-.08.66-.14 1.32-.14 2s.06 1.34.14 2H4.26zm.82 2h2.95c.32 1.25.78 2.45 1.38 3.56-1.84-.63-3.37-1.9-4.33-3.56zm2.95-8H5.08c.96-1.66 2.49-2.93 4.33-3.56C8.81 5.55 8.35 6.75 8.03 8zM12 19.96c-.83-1.2-1.48-2.53-1.91-3.96h3.82c-.43 1.43-1.08 2.76-1.91 3.96zM14.34 14H9.66c-.09-.66-.16-1.32-.16-2s.07-1.35.16-2h4.68c.09.65.16 1.32.16 2s-.07 1.34-.16 2zm.25 5.56c.6-1.11 1.06-2.31 1.38-3.56h2.95c-.96 1.65-2.49 2.93-4.33 3.56zM16.36 14c.08-.66.14-1.32.14-2s-.06-1.34-.14-2h3.38c.16.64.26 1.31.26 2s-.1 1.36-.26 2h-3.38z"
            ]
            []
        ]
        |> wrap


close : Int -> Color -> Element msg
close size color =
    svg
        [ viewBox "0 0 24 24"
        , height size
        , width size
        ]
        [ Svg.path
            [ fill <| rgb color
            , d "M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12 19 6.41z"
            ]
            []
        ]
        |> wrap


spinner : Int -> Color -> Element msg
spinner size color =
    svg
        [ viewBox "0 0 512 512"
        , height size
        , width size
        ]
        [ Svg.path [ fill <| rgb color, d "M288 39.056v16.659c0 10.804 7.281 20.159 17.686 23.066C383.204 100.434 440 171.518 440 256c0 101.689-82.295 184-184 184-101.689 0-184-82.295-184-184 0-84.47 56.786-155.564 134.312-177.219C216.719 75.874 224 66.517 224 55.712V39.064c0-15.709-14.834-27.153-30.046-23.234C86.603 43.482 7.394 141.206 8.003 257.332c.72 137.052 111.477 246.956 248.531 246.667C393.255 503.711 504 392.788 504 256c0-115.633-79.14-212.779-186.211-240.236C302.678 11.889 288 23.456 288 39.056z" ] [] ]
        |> wrap


eth : Int -> Color -> Element msg
eth size color =
    svg
        [ viewBox "0 0 67.2 103.67"
        , height size
        , width size
        ]
        [ polygon [ points "34.44 76.07 60.29 60.82 34.44 97.13 34.44 76.07", fill <| rgb color ] []
        , polygon [ points "34.44 40.3 61.44 52.56 34.44 68.53 34.44 40.3", fill <| rgb color ] []
        , polygon [ points "34.44 38.45 34.44 6.27 61.15 50.58 34.44 38.45", fill <| rgb color ] []
        , polygon [ points "6.91 60.82 32.76 76.07 32.76 97.13 6.91 60.82", fill <| rgb color ] []
        , polygon [ points "5.75 52.56 32.76 40.3 32.76 68.53 5.75 52.56", fill <| rgb color ] []
        , polygon [ points "32.76 6.27 32.76 38.45 6.05 50.58 32.76 6.27", fill <| rgb color ] []
        ]
        |> wrap


xDai : Int -> Element msg
xDai size =
    svg [ viewBox "0 0 512 512", height size, width size ]
        [ Svg.path
            [ fill "#fff", d "M0 0v511h512V0H0z" ]
            []
        , Svg.path [ fill "#48a9a6", d "M40 40v86h172V40H40m258 0v86h172V40H298M40 298v172h172v-86h-86v-86H40m344 0v86h-86v86h172V298h-86z" ] []
        , Svg.path [ d "M0 511c4.4687 1.875 10.1913 1 15 1h497c-4.469-1.875-10.191-1-15-1H0z" ] []
        ]
        |> wrap


link : Int -> Color -> Element msg
link size color =
    svg [ height size, viewBox "0 0 24 24", width size ]
        [ Svg.path
            [ d "M0 0h24v24H0z"
            , fill "none"
            ]
            []
        , Svg.path
            [ d "M19 19H5V5h7V3H5c-1.11 0-2 .9-2 2v14c0 1.1.89 2 2 2h14c1.1 0 2-.9 2-2v-7h-2v7zM14 3v2h3.59l-9.83 9.83 1.41 1.41L19 6.41V10h2V3h-7z"
            , fill <| rgb color
            ]
            []
        ]
        |> wrap


dollar : Int -> Color -> Element msg
dollar size color =
    svg
        [ viewBox "0 0 67.2 97.1"
        , height size
        , width size
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
        , height size
        , width size
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
        , height size
        , width size
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
        , height size
        , width size
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
        , height size
        , width size
        ]
        [ path
            [ fill <| rgb color
            , d "M35 44L22 34 10 44V16H0V6a6 6 0 015-6 7 7 0 011 0h22a6 6 0 017 6 7 7 0 010 1v37zM10 13V3H6a3 3 0 00-1 1 3 3 0 00-2 2v7z"
            ]
            []
        ]
        |> wrap


logo : Int -> Element msg
logo size =
    svg
        [ viewBox "0 0 178.6 230.13"
        , width size
        ]
        [ Svg.defs [] [ Svg.style [] [ Svg.text ".cls-1{fill:#f69b1e}.cls-3{fill:#a6a7aa}" ] ]
        , Svg.path [ d "M97.7 169.36c1.67-2.2 7.06-7.72 7.06-18 0-14.58-18.75-22-12.64-38.67-11.81 6.26-12.81 26-8.79 32.22-3.86-1-6.37-6-6-10.12-6.36 7.52-7.65 20.72-.21 30.48 1.06 1.39 2.17 2.74 3.26 4.11S58.3 161.16 58.3 139c0-34.35 31.54-35.09 24.81-68.74 15.21 11.55 19.53 35.77 19.53 50.12 6.86-3.24 7.66-13.5 7.71-13.85.19.15 10.87 13.87 10.87 31.8 0 23.61-23.36 30.91-23.55 31z", Svg.Attributes.class "cls-1" ] []
        , Svg.path [ fill "#808083", d "M89.3 38.12A52.76 52.76 0 0051.82 128a48.56 48.56 0 015.12-13.92 39.78 39.78 0 0115.39-59.21 40 40 0 011.89-.83 39.82 39.82 0 0148.5 58.52 65 65 0 014.89 14.61A52.75 52.75 0 0089.3 38.12z" ] []
        , Svg.path [ d "M52.89 151.56a70.76 70.76 0 1173.86-.64c-3.43 10.06-10.69 17-17.65 21.32a83.7 83.7 0 10-39.23.09c-6.57-4.28-13.56-11.16-16.98-20.77z", Svg.Attributes.class "cls-3" ] []
        , Svg.path [ d "M29.33 213.66L27.86 215a2.1 2.1 0 00-1.57-1.07 1 1 0 00-.64.21.58.58 0 00-.25.47.78.78 0 00.18.49 13.32 13.32 0 001.42 1.29c.76.63 1.21 1 1.38 1.19a4.29 4.29 0 01.85 1.17 2.85 2.85 0 01.25 1.21 2.78 2.78 0 01-.88 2.12 3.27 3.27 0 01-2.31.83 3.45 3.45 0 01-1.94-.54 4.38 4.38 0 01-1.35-1.71l1.66-1c.5.92 1.08 1.38 1.73 1.38a1.27 1.27 0 00.86-.3.88.88 0 00.35-.68 1.17 1.17 0 00-.27-.71 7 7 0 00-1.15-1.08 11.72 11.72 0 01-2.2-2.14 2.77 2.77 0 01-.5-1.51 2.48 2.48 0 01.83-1.86 2.89 2.89 0 012-.77 3.2 3.2 0 011.48.36 5.4 5.4 0 011.54 1.31zM50.65 212.24h2v10.41h-2zM81.69 213.94l-1.4 1.38a4.13 4.13 0 00-3.08-1.4 3.53 3.53 0 00-2.58 1 3.34 3.34 0 00-1 2.47 3.43 3.43 0 001.04 2.61 3.66 3.66 0 002.66 1 3.24 3.24 0 001.73-.44 3 3 0 001.14-1.35h-3v-1.86h5.24v.44a5 5 0 01-.71 2.59 4.91 4.91 0 01-1.83 1.87 5.21 5.21 0 01-2.64.64 5.86 5.86 0 01-2.89-.7 5.11 5.11 0 01-2-2 5.62 5.62 0 01-.74-2.81A5.23 5.23 0 0173 213.8a5.39 5.39 0 014.2-1.8 6.54 6.54 0 012.54.5 6 6 0 011.95 1.44zM98.25 212.24h1.9l4.45 6.85v-6.85h2v10.41h-1.9l-4.45-6.83v6.83h-2zM126.81 212.24h2l4 10.41h-2.06l-.82-2.14h-4.23l-.85 2.14h-2.06zm1 2.76l-1.39 3.58h2.78zM150.76 212.24h2v8.52h2.89v1.89h-4.87z", Svg.Attributes.class "cls-1" ] []
        , Svg.path [ d "M68.15 204.53h3.78l-2.99-21.22h-3.83l-3.76 13.99-3.75-13.99h-3.83l-3 21.22h3.79l1.67-11.85 3.18 11.85h3.89l3.18-11.85 1.67 11.85z", Svg.Attributes.class "cls-3" ] []
        , Svg.path [ d "M89.23 204.53a10.61 10.61 0 1110.61-10.61 10.62 10.62 0 01-10.61 10.61zm0-17.47a6.86 6.86 0 106.86 6.86 6.86 6.86 0 00-6.86-6.86z", Svg.Attributes.class "cls-1" ] []
        , Svg.path [ d "M114.17 194.34l8.58 10.19h4.9l-10.57-12.55 10.64-8.6v-.07h-5.87l-11.62 9.39v-9.39h-3.75v21.22h3.75v-7l3.94-3.19zM155.63 187.06v-3.75h-21.32v21.22h21.32v-3.75h-17.57v-4.99h17.57v-3.75h-17.57v-4.98h17.57zM33.71 204.54c-6.23 0-10.74-2.67-10.74-6.35h3.9c0 .73 2.32 2.45 6.84 2.45 3.64 0 6.39-1.06 6.39-2.45a1.31 1.31 0 00-.4-1c-.51-.5-2-1.33-6-1.31h-.37c-1.55 0-6.28 0-8.79-2.44a5.2 5.2 0 01-1.54-3.79c0-3.86 4-6.35 10.28-6.35S44 186 44 189.65h-3.9c0-.73-2.33-2.45-6.85-2.45-3.64 0-6.38 1.05-6.38 2.45a1.33 1.33 0 00.39 1c.51.5 2 1.31 6 1.31h.37c1.55 0 6.28-.05 8.8 2.44a5.22 5.22 0 011.57 3.79c0 3.86-4 6.35-10.29 6.35z", Svg.Attributes.class "cls-3" ] []
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


height : Int -> Svg.Attribute msg
height =
    String.fromInt >> Svg.Attributes.height


width : Int -> Svg.Attribute msg
width =
    String.fromInt >> Svg.Attributes.width
