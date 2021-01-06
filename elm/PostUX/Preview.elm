module PostUX.Preview exposing (..)

import Common.Msg
import Common.Types exposing (..)
import Common.View exposing (daiAmountInput, daiSymbol, unlockUXOr)
import Dict exposing (Dict)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
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
    -> Wallet
    -> Maybe Model
    -> Post.Published
    -> Element Msg
view dProfile donateChecked showAddressOnPhace blockTimes now wallet maybeUXModel post =
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
        [ mainPreviewPane
            dProfile
            showAddressOnPhace
            donateChecked
            blockTimes
            now
            (Wallet.unlockStatus wallet)
            maybeUXModel
            post
        ]


mainPreviewPane :
    DisplayProfile
    -> Bool
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> UnlockStatus
    -> Maybe Model
    -> Post.Published
    -> Element Msg
mainPreviewPane dProfile showAddress donateChecked blockTimes now unlockStatus maybeUXModel post =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 10
        ]
        [ previewMetadata dProfile blockTimes now post
        , previewBody dProfile showAddress post
        , publishedPostActionForm
            dProfile
            donateChecked
            post
            (maybeUXModel
                |> Maybe.map .showInput
                |> Maybe.withDefault None
            )
            unlockStatus
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


publishedPostActionForm :
    DisplayProfile
    -> Bool
    -> Post.Published
    -> ShowInputState
    -> UnlockStatus
    -> Element Msg
publishedPostActionForm dProfile donateChecked publishedPost showInput unlockStatus =
    Element.el
        [ Element.alignRight ]
    <|
        case showInput of
            None ->
                Element.row
                    [ Element.spacing 5
                    ]
                    [ supportTipButton publishedPost.id
                    , supportBurnButton publishedPost.id
                    , replyButton publishedPost.id
                    ]

            Tip input ->
                unlockOrInputForm
                    dProfile
                    donateChecked
                    (Element.rgba 0 1 0 0.1)
                    input
                    "Tip"
                    (SupportTipSubmitClicked publishedPost.id)
                    unlockStatus

            Burn input ->
                unlockOrInputForm
                    dProfile
                    donateChecked
                    (Element.rgba 1 0 0 0.1)
                    input
                    "Burn"
                    (SupportBurnSubmitClicked publishedPost.id)
                    unlockStatus


supportTipButton :
    Post.Id
    -> Element Msg
supportTipButton postId =
    publishedPostActionButton
        [ EH.withTitle "Tip DAI for this post, rewarding the author" ]
        SupportTipClicked
    <|
        Element.image
            [ Element.height <| Element.px 10
            , Element.centerX
            ]
            { src = "img/dai-unit-char-green.svg"
            , description = "support green"
            }


supportBurnButton :
    Post.Id
    -> Element Msg
supportBurnButton postId =
    publishedPostActionButton
        [ EH.withTitle "Burn DAI to increase this post's visibility" ]
        SupportBurnClicked
    <|
        Element.image
            [ Element.height <| Element.px 10
            , Element.centerX
            ]
            { src = "img/dai-unit-char-red.svg"
            , description = "support burn"
            }


replyButton :
    Post.Id
    -> Element Msg
replyButton postId =
    publishedPostActionButton
        [ EH.withTitle "Reply" ]
        (MsgUp <| Common.Msg.StartInlineCompose <| Post.Reply postId)
    <|
        Element.image
            [ Element.width Element.fill
            , Element.height <| Element.px 10
            ]
            { src = "img/reply-arrow.svg"
            , description = "reply"
            }


publishedPostActionButton :
    List (Attribute Msg)
    -> Msg
    -> Element Msg
    -> Element Msg
publishedPostActionButton attributes onClick innerEl =
    Element.el
        (attributes
            ++ [ Element.padding 3
               , Element.pointer
               , Element.Border.rounded 1
               , Element.Background.color <| Element.rgba 1 1 1 0.3
               , Element.Border.shadow
                    { offset = ( 0, 0 )
                    , size = 0
                    , blur = 5
                    , color = Element.rgba 0 0 0 0.1
                    }
               , Element.Events.onClick onClick
               , Element.width <| Element.px 15
               , Element.height <| Element.px 15
               ]
        )
        innerEl


unlockOrInputForm :
    DisplayProfile
    -> Bool
    -> Element.Color
    -> String
    -> String
    -> (TokenValue -> Msg)
    -> UnlockStatus
    -> Element Msg
unlockOrInputForm dProfile donateChecked bgColor currentString buttonLabel onSubmit unlockStatus =
    Element.row
        [ Element.padding 10
        , Element.Border.rounded 6
        , Element.Background.color bgColor
        , Element.spacing 10
        , Element.Border.glow
            (Element.rgba 0 0 0 0.1)
            5
        ]
        [ unlockUXOr
            dProfile
            []
            unlockStatus
            MsgUp
            (inputForm dProfile donateChecked currentString buttonLabel onSubmit)
        , EH.closeButton
            [ Element.alignTop
            , Element.moveUp 5
            , Element.moveRight 5
            ]
            EH.black
            ResetActionForm
        ]


inputForm :
    DisplayProfile
    -> Bool
    -> String
    -> String
    -> (TokenValue -> Msg)
    -> Element Msg
inputForm dProfile donateChecked currentString buttonLabel onSubmit =
    Element.column
        [ Element.spacing 10 ]
        [ Element.row
            [ Element.spacing 10
            , Element.centerX
            ]
            [ daiSymbol False
                [ Element.height <| Element.px 22 ]
            , daiAmountInput
                dProfile
                []
                currentString
                AmountInputChanged
            , maybeSubmitButton
                dProfile
                buttonLabel
                (TokenValue.fromString currentString)
                onSubmit
            ]
        , Element.row
            [ Element.centerX
            , Element.Font.size 12
            ]
            [ Element.Input.checkbox
                []
                { onChange = MsgUp << Common.Msg.DonationCheckboxSet
                , icon = Element.Input.defaultCheckbox
                , checked = donateChecked
                , label =
                    Element.Input.labelRight
                        [ Element.centerY
                        ]
                    <|
                        Element.text "Donate an extra 1% to "
                }
            , Element.newTabLink
                [ Element.Font.color theme.linkTextColor
                , Element.centerY
                ]
                { url = "https://foundrydao.com/"
                , label = Element.text "Foundry"
                }
            ]
        ]


maybeSubmitButton :
    DisplayProfile
    -> String
    -> Maybe TokenValue
    -> (TokenValue -> Msg)
    -> Element Msg
maybeSubmitButton dProfile label maybeAmount onSubmit =
    case maybeAmount of
        Just amount ->
            theme.emphasizedActionButton
                EH.Mobile
                []
                [ label ]
                (EH.Action <| onSubmit amount)

        Nothing ->
            Theme.disabledButton
                EH.Mobile
                []
                label


previewMaxTextLength : Int
previewMaxTextLength =
    250


limitedString : String -> String
limitedString text =
    if String.length text > previewMaxTextLength then
        String.slice 0 previewMaxTextLength text ++ "..."

    else
        text
