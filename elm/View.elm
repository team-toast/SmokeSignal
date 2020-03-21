module View exposing (root)

import Dict exposing (Dict)
import Browser
import CommonTypes exposing (..)
import Dict.Extra
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH
import Markdown
import TokenValue exposing (TokenValue)
import Types exposing (..)


root : Model -> Browser.Document Msg
root model =
    { title = "SmokeSig"
    , body =
        [ Element.layout
            [ Element.width Element.fill ]
          <|
            body model
        ]
    }


body : Model -> Element Msg
body model =
    Element.column
        [ Element.width Element.fill
        , Element.padding 20
        , Element.spacing 30
        ]
        [ title
        , viewMessages model.messages
        ]


title : Element Msg
title =
    Element.el
        [ Element.Font.size 40
        , Element.Font.bold
        ]
    <|
        Element.text "SmokeSig"


viewMessages : List Message -> Element Msg
viewMessages messages =
    sortMessagesByBlock messages
        |> Dict.toList
        |> List.map
            (\( blocknum, messagesForBlock ) ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ Element.el
                        [ Element.Font.size 14
                        , Element.Font.italic
                        ]
                        (Element.text <| String.fromInt blocknum)
                    , Element.column
                        [ Element.paddingXY 20 0 ]
                        (List.map viewMessage messagesForBlock)
                    ]
            )
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing 20
            ]


sortMessagesByBlock : List Message -> Dict Int (List Message)
sortMessagesByBlock messages =
    messages
        |> Dict.Extra.groupBy
            .block


viewMessage : Message -> Element Msg
viewMessage message =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ Element.row
            [ Element.spacing 10 ]
            [ viewDaiBurned message.burnAmount
            , viewAuthor message.from
            ]
        , viewMessageContent message.message
        ]


viewDaiBurned : TokenValue -> Element Msg
viewDaiBurned amount =
    Element.el
        [ Element.Font.color EH.blue
        , Element.Font.size 20
        ]
        (Element.text <| TokenValue.toConciseString amount)


viewAuthor : Address -> Element Msg
viewAuthor fromAddress =
    Element.el
        [ Element.Font.size 20 ]
        (Element.text <| Eth.Utils.addressToString fromAddress)


viewMessageContent : String -> Element Msg
viewMessageContent content =
    renderMarkdownParagraphs
        [ Element.spacing 2
        , Element.padding 10
        , Element.Border.rounded 10
        , Element.Background.color EH.softRed
        ]
        content


renderMarkdownParagraphs : List (Attribute Msg) -> String -> Element Msg
renderMarkdownParagraphs attributes =
    Markdown.toHtml Nothing
        >> List.map Element.html
        >> Element.paragraph
            attributes
