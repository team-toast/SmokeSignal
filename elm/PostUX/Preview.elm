module PostUX.Preview exposing (..)

import Common.Msg
import Common.Types exposing (..)
import Common.View exposing (daiSymbol)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
import Post
import PostUX.Types exposing (..)
import PostUX.View
import Theme
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)
import Html.Attributes


view :
    DisplayProfile
    -> Bool
    -> Bool
    -> Maybe Model
    -> Post.Published
    -> Element Msg
view dProfile donateChecked showAddressOnPhace maybeUXModel publishedPost =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px <| 100
        , Element.spacing 20
        , Element.clipX
        , Element.clipY
        , Element.Background.color Theme.darkGray
        , Element.Border.rounded 10
        , Element.padding 10
        ]
        [ daiBurnedPane dProfile publishedPost
        , mainPreviewPane dProfile showAddressOnPhace publishedPost
        ]


daiBurnedPane :
    DisplayProfile
    -> Post.Published
    -> Element Msg
daiBurnedPane dProfile publishedPost =
    Element.row
        [ Element.spacing 3 ]
        [ daiSymbol False [ Element.height <| Element.px 18 ]
        , Element.text <| TokenValue.toConciseString <| Post.totalBurned <| Post.PublishedPost publishedPost
        ]


mainPreviewPane :
    DisplayProfile
    -> Bool
    -> Post.Published
    -> Element Msg
mainPreviewPane dProfile showAddress publishedPost =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 5
        ]
        [ previewMetadata dProfile publishedPost
        , previewBody dProfile showAddress publishedPost
        ]


previewMetadata :
    DisplayProfile
    -> Post.Published
    -> Element Msg
previewMetadata dProfile publishedPost =
    Element.row
        [ Element.spaceEvenly
        , Element.spacing 5
        , Element.Font.size <| responsiveVal dProfile 16 10
        ]
        [ viewContext dProfile publishedPost.core.metadata.context
        , viewTiming dProfile publishedPost.id
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
    -> Post.Id
    -> Element Msg
viewTiming dProfile id =
    Element.text <| String.fromInt id.block


viewDaiTipped :
    DisplayProfile
    -> TokenValue
    -> Element Msg
viewDaiTipped dProfile amount =
    Element.row
        [ Element.spacing 3 ]
        [ daiSymbol False [ Element.height <| Element.px 18 ]
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
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , Element.clipX
        , Element.clipY
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
    Element.el
        [ Element.clipX
        , Element.clipY
        , Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        case content.title of
            Just title ->
                Element.text title

            Nothing ->
                Element.text content.body
