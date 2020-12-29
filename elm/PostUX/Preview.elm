module PostUX.Preview exposing (..)

import Common.Msg
import Common.Types exposing (..)
import Common.View exposing (daiSymbol)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Helpers.Element as EH exposing (DisplayProfile, limitedString, responsiveVal)
import Helpers.Time as TimeHelpers
import Html.Attributes
import Post
import PostUX.Types exposing (..)
import PostUX.View
import Theme exposing (almostWhite, lightGray, theme)
import Time
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view :
    DisplayProfile
    -> Bool
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Model
    -> Post.Published
    -> Element Msg
view dProfile donateChecked showAddressOnPhace blockTimes now maybeUXModel post =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px <| 120
        , Element.spacing 20
        , Element.Background.color theme.blockBackground
        , Element.Border.width 1
        , Element.Border.color theme.blockBorderColor
        , Element.Border.rounded 5
        , Element.padding 10
        ]
        [ mainPreviewPane dProfile showAddressOnPhace blockTimes now post
        ]


mainPreviewPane :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Post.Published
    -> Element Msg
mainPreviewPane dProfile showAddress blockTimes now post =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 10
        ]
        [ previewMetadata dProfile blockTimes now post
        , previewBody dProfile showAddress post
        ]


previewMetadata :
    DisplayProfile
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Post.Published
    -> Element Msg
previewMetadata dProfile blockTimes now post =
    Element.row
        [ Element.Font.size <| responsiveVal dProfile 16 10
        , Element.width <| Element.px 300
        , Element.spacing 40
        ]
        [ viewAccounting dProfile post
        , viewTiming dProfile blockTimes now post.id
        , viewContext dProfile post.core.metadata.context
        ]


viewAccounting :
    DisplayProfile
    -> Post.Published
    -> Element Msg
viewAccounting dProfile post =
    Element.row
        [ Element.width <| Element.px 100
        , Element.spacing 5
        ]
        [ viewDaiBurned dProfile post
        , Maybe.map (viewDaiTipped dProfile)
            (post.maybeAccounting |> Maybe.map .totalTipped)
            |> Maybe.withDefault Element.none
        ]


commonDaiElStyles : List (Element.Attribute Msg)
commonDaiElStyles =
    [ Element.spacing 3
    , Element.padding 3
    , Element.Border.rounded 3
    , Element.Font.size 14
    ]


viewDaiBurned :
    DisplayProfile
    -> Post.Published
    -> Element Msg
viewDaiBurned dProfile post =
    Element.row
        (commonDaiElStyles
            ++ [ Element.Background.color theme.daiBurnedBackground ]
        )
        [ daiSymbol True [ Element.height <| Element.px 14 ]
        , Element.el
            [ Element.Font.color EH.white ]
          <|
            Element.text <|
                TokenValue.toConciseString <|
                    Post.totalBurned <|
                        Post.PublishedPost post
        ]


viewDaiTipped :
    DisplayProfile
    -> TokenValue
    -> Element Msg
viewDaiTipped dProfile amount =
    Element.row
        (commonDaiElStyles
            ++ [ Element.Background.color theme.daiTippedBackground ]
        )
        [ daiSymbol True [ Element.height <| Element.px 14 ]
        , Element.el
            [ Element.Font.color EH.white ]
          <|
            Element.text <|
                TokenValue.toConciseString amount
        ]


viewContext :
    DisplayProfile
    -> Post.Context
    -> Element Msg
viewContext dProfile context =
    Element.el
        [ Element.width Element.fill
        , Element.Font.color almostWhite
        ]
    <|
        case context of
            Post.Reply id ->
                Element.text "reply"

            Post.TopLevel topic ->
                Element.text <| "#" ++ topic


viewTiming :
    DisplayProfile
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Post.Id
    -> Element Msg
viewTiming dProfile blockTimes now id =
    let
        maybePostTime =
            blockTimes
                |> Dict.get id.block

        maybeTimePassed =
            maybePostTime
                |> Maybe.map
                    (\postTime ->
                        TimeHelpers.sub now postTime
                    )
    in
    Element.el
        [ Element.width <| Element.px 100
        , Element.Font.color theme.subtleTextColor
        ]
    <|
        Element.text
            (maybeTimePassed
                |> Maybe.map TimeHelpers.roundToSingleUnit
                |> Maybe.map (\s -> s ++ " ago")
                |> Maybe.withDefault "..."
            )


previewBody :
    DisplayProfile
    -> Bool
    -> Post.Published
    -> Element Msg
previewBody dProfile showAddress post =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 5
        , Element.clip
        ]
        [ Common.View.phaceElement
            ( 60, 60 )
            True
            post.core.author
            showAddress
            (MsgUp <| Common.Msg.ShowOrHideAddress <| PhaceForPublishedPost post.id)
            NoOp
        , viewTitleOrTextPreview dProfile post.core.content
        ]


viewTitleOrTextPreview :
    DisplayProfile
    -> Post.Content
    -> Element Msg
viewTitleOrTextPreview dProfile content =
    Element.el
        [ Element.Font.color almostWhite
        , Element.Font.size (responsiveVal dProfile 14 8)
        ]
    <|
        Element.text <|
            limitedString <|
                case content.title of
                    Just title ->
                        title

                    Nothing ->
                        content.body
