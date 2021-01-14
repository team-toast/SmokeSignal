module View.Common exposing (renderContentOrError)

import Element exposing (Element)
import Element.Font as Font
import ElementMarkdown
import Theme exposing (theme)
import Types exposing (Content, Msg)


renderContentOrError : Content -> Element Msg
renderContentOrError content =
    let
        renderResult =
            ElementMarkdown.renderString
                [ Element.spacing 15
                , Font.color theme.postBodyTextColor
                , Element.width Element.fill
                ]
                content.body
    in
    case renderResult of
        Ok rendered ->
            rendered

        Err errStr ->
            Element.el
                [ Font.color theme.errorTextColor
                , Font.italic
                ]
            <|
                Element.text <|
                    "Error parsing/rendering markdown: "
                        ++ errStr
