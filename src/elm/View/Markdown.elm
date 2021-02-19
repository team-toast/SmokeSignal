module View.Markdown exposing (renderString)

import Element exposing (Element, column, fill, height, row, spacing, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Helpers.Element as EH
import Html
import Html.Attributes
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer exposing (Renderer)
import View.Attrs exposing (hover)



-- Largely taken from https://github.com/dillonkearns/elm-markdown/blob/master/examples/src/ElmUi.elm


renderString : String -> Element msg
renderString src =
    src
        |> Markdown.Parser.parse
        |> Result.mapError (always ())
        --|> Result.mapError
        --(List.map Markdown.Parser.deadEndToString
        -->> String.join "\n"
        --)
        |> Result.andThen
            (Markdown.Renderer.render renderer
                >> Result.mapError (always ())
            )
        |> Result.withDefault [ Element.text "There has been a problem." ]
        |> column [ spacing 10, height fill, width fill ]


renderer : Renderer (Element msg)
renderer =
    { heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacing 3 ]
    , thematicBreak =
        Element.el
            [ Element.width Element.fill
            , Element.paddingXY 10 5
            ]
        <|
            EH.thinHRuler (Element.rgba 0 0 0 0.5)
    , text = Element.text
    , strong = \content -> row [ Element.Font.bold ] content
    , emphasis = \content -> row [ Element.Font.italic ] content
    , codeSpan = code
    , link =
        \{ destination } body ->
            Element.newTabLink
                [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex")
                , hover
                ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Element.Font.color (Element.rgb255 0 0 255)
                        ]
                        body
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
                                                Element.text "•"
                                    , Element.paragraph [ Element.width Element.fill ] children
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
                                        Element.text (String.fromInt (index + startingIndex))
                                    , Element.paragraph
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
            Element.paragraph [] children
    , tableCell = \_ -> Element.paragraph []
    }


heading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText } =
    Element.paragraph
        [ Element.Font.size
            (case level of
                Markdown.Block.H1 ->
                    42

                Markdown.Block.H2 ->
                    36

                Markdown.Block.H3 ->
                    30

                Markdown.Block.H4 ->
                    24

                _ ->
                    20
            )
        , Element.Font.bold
        ]
        [ Element.text rawText ]


code : String -> Element msg
code snippet =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        , Element.scrollbarX
        , Element.Border.rounded 2
        , Element.paddingXY 5 3
        , Element.Font.family
            [ Element.Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.padding 20
        , Element.Font.family
            [ Element.Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text details.body)
