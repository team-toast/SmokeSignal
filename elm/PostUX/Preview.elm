module PostUX.Preview exposing (..)

import Common.Msg
import Common.Types exposing (..)
import Common.View exposing (daiSymbol)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
import Helpers.Time as TimeHelpers
import Html.Attributes
import Post
import PostUX.Types exposing (..)
import PostUX.View
import Theme exposing (theme)
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
view dProfile donateChecked showAddressOnPhace blockTimes now maybeUXModel publishedPost =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px <| 100
        , Element.spacing 20
        , Element.Background.color theme.blockBackground
        , Element.Border.width 1
        , Element.Border.color theme.blockBorderColor 
        , Element.Border.rounded 5
        , Element.padding 10
        ]
        [ daiBurnedPane dProfile publishedPost
        , mainPreviewPane dProfile showAddressOnPhace blockTimes now publishedPost
        ]


daiBurnedPane :
    DisplayProfile
    -> Post.Published
    -> Element Msg
daiBurnedPane dProfile publishedPost =
    Element.row
        [ Element.spacing 3 ]
        [ daiSymbol True [ Element.height <| Element.px 18 ]
        , Element.text <| TokenValue.toConciseString <| Post.totalBurned <| Post.PublishedPost publishedPost
        ]


mainPreviewPane :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Post.Published
    -> Element Msg
mainPreviewPane dProfile showAddress blockTimes now publishedPost =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 5
        ]
        [ previewMetadata dProfile blockTimes now publishedPost
        , previewBody dProfile showAddress publishedPost
        ]


previewMetadata :
    DisplayProfile
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Post.Published
    -> Element Msg
previewMetadata dProfile blockTimes now publishedPost =
    Element.row
        [ Element.spaceEvenly
        , Element.Font.size <| responsiveVal dProfile 16 10
        , Element.width <| Element.px 300
        ]
        
        [ viewContext dProfile publishedPost.core.metadata.context
        , viewTiming dProfile blockTimes now publishedPost.id
        , Maybe.map
            (viewDaiTipped dProfile)
            (publishedPost.maybeAccounting |> Maybe.map .totalTipped)
            |> Maybe.withDefault Element.none
        ]


viewContext :
    DisplayProfile
    -> Post.Context
    -> Element Msg
viewContext dProfile context =
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
    Element.text
        (maybeTimePassed
            |> Maybe.map TimeHelpers.roundToSingleUnit
            |> Maybe.map (\s -> s ++ " ago")
            |> Maybe.withDefault "..."
        )


viewDaiTipped :
    DisplayProfile
    -> TokenValue
    -> Element Msg
viewDaiTipped dProfile amount =
    Element.row
        [ Element.spacing 3 ]
        [ daiSymbol True [ Element.height <| Element.px 18 ]
        , Element.text <| TokenValue.toConciseString amount
        ]


previewBody :
    DisplayProfile
    -> Bool
    -> Post.Published
    -> Element Msg
previewBody dProfile showAddress publishedPost =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 5
        , Element.clip
        ]
        [ Common.View.phaceElement True
            publishedPost.core.author
            showAddress
            (MsgUp <| Common.Msg.ShowOrHideAddress <| PhaceForPublishedPost publishedPost.id)
            NoOp
        , viewTitleOrTextPreview dProfile publishedPost.core.content
        ]


viewTitleOrTextPreview :
    DisplayProfile
    -> Post.Content
    -> Element Msg
viewTitleOrTextPreview dProfile content =
    case content.title of
        Just title ->
            Element.text title

        Nothing ->
            Element.text content.body
