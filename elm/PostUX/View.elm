module PostUX.View exposing (..)

import Common.Msg
import Common.Types exposing (..)
import Common.View exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Lazy
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile)
import Helpers.Eth as EthHelpers
import List.Extra
import Maybe.Extra
import Post exposing (Post)
import PostUX.Types exposing (..)
import Routing exposing (Route)
import Theme exposing (defaultTheme)
import Time
import TokenValue exposing (TokenValue)


view : DisplayProfile -> Bool -> Post -> Maybe Model -> Element Msg
view dProfile showContext post maybeUXModel =
    let
        postCore =
            Post.getCore post

        maybePostId =
            case post of
                Post.PublishedPost publishedPost ->
                    Just publishedPost.id

                _ ->
                    Nothing
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ case dProfile of
            Desktop ->
                Element.el
                    [ Element.alignTop
                    , Element.height <| Element.px 100
                    ]
                <|
                    makePhaceElement
                        postCore.author
                        maybeUXModel

            Mobile ->
                Element.none
        , Element.column
            [ Element.width Element.fill
            , Element.alignTop
            , Element.clipX
            ]
            [ Element.row
                [ Element.width Element.fill ]
                [ viewDaiBurned post
                , Maybe.map viewPostLinks maybePostId
                    |> Maybe.withDefault Element.none
                ]
            , viewMainPostBlock dProfile showContext post maybeUXModel
            ]
        ]


makePhaceElement : Address -> Maybe Model -> Element Msg
makePhaceElement author maybeUXModel =
    phaceElement
        True
        author
        (maybeUXModel
            |> Maybe.map .showAddress
            |> Maybe.withDefault False
        )
        ShowOrHideAuthorAddress
        NoOp


viewDaiBurned : Post -> Element Msg
viewDaiBurned post =
    Element.el
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.Background.color defaultTheme.daiBurnedBackground
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignLeft
        ]
    <|
        Element.row
            [ Element.spacing 3
            , EH.withTitle (burnSummaryString post)
            ]
            [ daiSymbol defaultTheme.daiBurnedTextIsWhite [ Element.height <| Element.px 18 ]
            , Element.text <| (Post.totalBurned post |> TokenValue.toConciseString)
            ]


burnSummaryString : Post -> String
burnSummaryString post =
    let
        authorBurned =
            Post.getCore post |> .authorBurn

        totalBurned =
            Post.totalBurned post

        crowdBurned =
            TokenValue.sub
                totalBurned
                authorBurned
    in
    "Author burned $"
        ++ (authorBurned |> TokenValue.toConciseString)
        ++ (if TokenValue.isZero crowdBurned then
                ""

            else
                ", Crowd burned $" ++ (crowdBurned |> TokenValue.toConciseString)
           )


viewPostLinks : Post.Id -> Element Msg
viewPostLinks postId =
    let
        route =
            Routing.ViewContext <|
                Post.ForPost postId
    in
    Element.row
        [ Element.alignBottom
        , Element.paddingXY 10 5
        , Element.Font.size 20
        , Element.Background.color defaultTheme.postBodyBackground
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignRight
        , Element.spacing 20
        ]
        [ Element.el
            [ Element.Font.color defaultTheme.linkTextColor
            , Element.pointer
            , Element.Font.bold
            , Element.Events.onClick <|
                MsgUp <|
                    Common.Msg.GotoRoute <|
                        route
            ]
            (Element.text (shortenedHash postId.messageHash))
        , Element.newTabLink
            [ Element.Font.color defaultTheme.linkTextColor
            , Element.Font.size 16
            ]
            { url =
                Routing.routeToFullDotEthUrlString <|
                    Routing.ViewContext <|
                        Post.ForPost postId
            , label = Element.text "(.eth permalink)"
            }
        ]


viewMainPostBlock : DisplayProfile -> Bool -> Post -> Maybe Model -> Element Msg
viewMainPostBlock dProfile showContext post maybeUXModel =
    let
        postCore =
            Post.getCore post
    in
    Element.column
        [ Element.width Element.fill
        , Element.scrollbarX
        , Element.clipY
        , Element.padding 20
        , Element.spacing 20
        , Element.Background.color (Element.rgb 0.8 0.8 1)
        , Element.Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomRight = 10
            , bottomLeft = 10
            }
        , Element.alignTop
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ case dProfile of
                Desktop ->
                    Element.none

                Mobile ->
                    makePhaceElement
                        postCore.author
                        maybeUXModel
            , Element.map MsgUp <| viewMetadata showContext postCore.metadata
            ]
        , Element.map never postCore.renderedPost
        , case post of
            Post.PublishedPost published ->
                publishedPostActions published

            _ ->
                Element.none
        ]


publishedPostActions : Post.Published -> Element Msg
publishedPostActions publishedPost =
    Element.row
        [ Element.alignRight ]
        [ Element.text "tip button here!"
        , replyButton publishedPost.id
        ]


replyButton : Post.Id -> Element Msg
replyButton postId =
    Element.el
        [ Element.padding 7
        , Element.pointer
        , Element.Border.rounded 4
        , Element.Background.color <| Element.rgba 1 1 1 0.3
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 5
            , color = Element.rgba 0 0 0 0.1
            }
        , Element.Events.onClick <| MsgUp <| Common.Msg.StartInlineCompose <| Post.ForPost postId
        , Element.width <| Element.px 30
        ]
    <|
        Element.image
            [ Element.width Element.fill ]
            { src = "img/reply-arrow.svg"
            , description = "reply"
            }



