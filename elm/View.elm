module View exposing (root)

import Browser
import Common.Types exposing (..)
import ComposeUX.View
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Lazy
import ElementMarkdown
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Helpers.Time as TimeHelpers
import Home.View
import Html.Attributes
import Json.Decode
import List.Extra
import Maybe.Extra
import Post exposing (Post)
import Phace
import Routing exposing (Route)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet


root : Model -> Browser.Document Msg
root model =
    { title = "SmokeSignal"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
            , Element.Events.onClick ClickHappened
            ]
          <|
            body model
        ]
    }


body : Model -> Element Msg
body model =
    let
        maybeUserInfoAndShowAddress =
            Wallet.userInfo model.wallet
                |> Maybe.map
                    (\userInfo ->
                        ( userInfo
                        , model.showAddress == Just User
                        )
                    )
    in
    Element.column
        ([ Element.width Element.fill
         , Element.height Element.fill
         ]
            ++ List.map
                Element.inFront
                (userNoticeEls
                    model.dProfile
                    model.userNotices
                )
        )
        [ header model.mode
        , case model.mode of
            Home ->
                Home.View.view model

            Compose ->
                Element.map ComposeUXMsg <|
                    ComposeUX.View.viewFull
                        model.dProfile
                        maybeUserInfoAndShowAddress
                        model.composeUXModel
                        (getMaybeTopic model)

            ViewAll ->
                viewAllPosts model

            ViewPost postId ->
                viewPostAndReplies postId model

            ViewTopic topic ->
                viewPostsForTopic topic model
        , if model.showHalfComposeUX then
            Element.el
                [ Element.width Element.fill
                , Element.alignBottom
                ]
                (Element.map ComposeUXMsg <|
                    ComposeUX.View.viewHalf
                        model.dProfile
                        maybeUserInfoAndShowAddress
                        model.composeUXModel
                        (getMaybeTopic model)
                )

          else
            Element.none
        ]


header : Mode -> Element Msg
header mode =
    Debug.todo "header"


userNoticeEls : EH.DisplayProfile -> List UserNotice -> List (Element Msg)
userNoticeEls dProfile notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft (20 |> EH.changeForMobile 5 dProfile)
            , Element.moveUp (20 |> EH.changeForMobile 5 dProfile)
            , Element.spacing (10 |> EH.changeForMobile 5 dProfile)
            , Element.alignRight
            , Element.alignBottom
            , Element.width <| Element.px (300 |> EH.changeForMobile 150 dProfile)
            , Element.Font.size (15 |> EH.changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map (userNotice dProfile)
            )
        , Element.column
            [ Element.moveRight (20 |> EH.changeForMobile 5 dProfile)
            , Element.moveDown 100
            , Element.spacing (10 |> EH.changeForMobile 5 dProfile)
            , Element.alignLeft
            , Element.alignTop
            , Element.width <| Element.px (300 |> EH.changeForMobile 150 dProfile)
            , Element.Font.size (15 |> EH.changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map (userNotice dProfile)
            )
        ]


userNotice : EH.DisplayProfile -> ( Int, UserNotice ) -> Element Msg
userNotice dProfile ( id, notice ) =
    let
        color =
            case notice.noticeType of
                UN.Update ->
                    Element.rgb255 100 200 255

                UN.Caution ->
                    Element.rgb255 255 188 0

                UN.Error ->
                    Element.rgb255 255 70 70

                UN.ShouldBeImpossible ->
                    Element.rgb255 200 200 200

        textColor =
            case notice.noticeType of
                UN.Error ->
                    Element.rgb 1 1 1

                _ ->
                    Element.rgb 0 0 0

        closeElement =
            Element.el
                [ Element.alignRight
                , Element.alignTop
                , Element.moveUp 5
                , Element.moveRight 5
                ]
                (EH.closeButton True (DismissNotice id))
    in
    Element.el
        [ Element.Background.color color
        , Element.Border.rounded (10 |> EH.changeForMobile 5 dProfile)
        , Element.padding (8 |> EH.changeForMobile 3 dProfile)
        , Element.width Element.fill
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.15
        , EH.subtleShadow
        , EH.onClickNoPropagation NoOp
        ]
        (notice.mainParagraphs
            |> List.map (List.map mapNever)
            |> List.indexedMap
                (\pNum paragraphLines ->
                    Element.paragraph
                        [ Element.width Element.fill
                        , Element.Font.color textColor
                        , Element.spacing 1
                        ]
                        (if pNum == 0 then
                            closeElement :: paragraphLines

                         else
                            paragraphLines
                        )
                )
            |> Element.column
                [ Element.spacing 4
                , Element.width Element.fill
                ]
        )


mapNever : Element Never -> Element Msg
mapNever =
    Element.map (always NoOp)


viewAllPosts =
    Debug.todo ""


viewPostAndReplies =
    Debug.todo ""


viewPostsForTopic =
    Debug.todo ""
