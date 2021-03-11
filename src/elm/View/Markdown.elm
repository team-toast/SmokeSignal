module View.Markdown exposing (renderString)

import Element exposing (Element, column, fill, height, paragraph, row, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Helpers.Element exposing (DisplayProfile, thinHRuler)
import Html
import Html.Attributes
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer exposing (Renderer)
import Theme
import View.Attrs exposing (hover)



-- Largely taken from https://github.com/dillonkearns/elm-markdown/blob/master/examples/src/ElmUi.elm


renderString : DisplayProfile -> String -> Element msg
renderString device src =
    let
        isMobile =
            device == Helpers.Element.Mobile

        fs =
            if isMobile then
                17

            else
                20

        sp =
            if isMobile then
                10

            else
                20
    in
    src
        |> Markdown.Parser.parse
        |> Result.mapError (always ())
        --|> Result.mapError
        --(List.map Markdown.Parser.deadEndToString
        -->> String.join "\n"
        --)
        |> Result.andThen
            (Markdown.Renderer.render (renderer device)
                >> Result.mapError (always ())
            )
        |> Result.withDefault [ text "There has been a problem." ]
        |> column [ spacing sp, height fill, width fill, Font.size fs ]


renderer : DisplayProfile -> Renderer (Element msg)
renderer device =
    { heading = heading device
    , paragraph =
        paragraph
            [ Element.spacing 3 ]
    , thematicBreak =
        thinHRuler (Element.rgba 0 0 0 0.5)
            |> Element.el
                [ Element.width Element.fill
                , Element.paddingXY 10 5
                ]
    , text = text
    , strong = \content -> row [ Font.bold ] content
    , emphasis = \content -> row [ Font.italic ] content
    , codeSpan = code
    , link =
        \{ destination } body ->
            Element.newTabLink
                [ hover, Font.color Theme.orange, Font.underline ]
                { url = destination
                , label =
                    body
                        |> paragraph []
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just _ ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.column
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgba 0 0 0 0.2)
                , Element.Background.color (Element.rgba 1 1 1 0.4)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(Markdown.Block.ListItem task children) ->
                            row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop
                                    , Element.width Element.fill
                                    , Element.spacing 15
                                    ]
                                    [ Element.el [ Element.alignTop ] <|
                                        case task of
                                            Markdown.Block.IncompleteTask ->
                                                Element.Input.defaultCheckbox False

                                            Markdown.Block.CompletedTask ->
                                                Element.Input.defaultCheckbox True

                                            Markdown.Block.NoTask ->
                                                text "•"
                                    , paragraph [ Element.width Element.fill ] children
                                    ]
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop
                                    , Element.width Element.fill
                                    , Element.spacing 15
                                    ]
                                    [ Element.el [ Element.alignTop ] <|
                                        text (String.fromInt (index + startingIndex))
                                    , paragraph
                                        [ Element.alignTop ]
                                        itemBlocks
                                    ]
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html = Markdown.Html.oneOf []
    , table = Element.column []
    , tableHeader = Element.column []
    , tableBody = Element.column []
    , tableRow = Element.row []
    , tableHeaderCell =
        \_ children ->
            paragraph [] children
    , tableCell = \_ -> paragraph []
    , strikethrough = paragraph [ Font.strike ]
    }


heading : DisplayProfile -> { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading device { level, rawText } =
    let
        isMobile =
            device == Helpers.Element.Mobile

        multiplier =
            case level of
                Markdown.Block.H1 ->
                    1.4

                Markdown.Block.H2 ->
                    1.3

                Markdown.Block.H3 ->
                    1.2

                Markdown.Block.H4 ->
                    1

                Markdown.Block.H5 ->
                    1

                Markdown.Block.H6 ->
                    1

        fs =
            (if isMobile then
                17

             else
                25
            )
                |> (*) multiplier
                |> round
    in
    [ text rawText ]
        |> paragraph
            [ Font.size fs
            , Font.bold
            ]


code : String -> Element msg
code snippet =
    text snippet
        |> Element.el
            [ Element.Background.color
                (Element.rgba 0 0 0 0.04)
            , Element.scrollbarX
            , Element.Border.rounded 2
            , Element.paddingXY 5 3
            , Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                    , name = "Source Code Pro"
                    }
                ]
            ]


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text details.body)
