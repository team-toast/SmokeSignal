module View.Post exposing (view)

import Dict exposing (Dict)
import Element exposing (Attribute, Element, el, text)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
import Helpers.Time as TimeHelpers
import Post
import Theme exposing (almostWhite, theme)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Common exposing (daiAmountInput, daiSymbol, unlockUXOr)
import Wallet


view :
    DisplayProfile
    -> Bool
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Wallet
    -> Types.PostState
    -> Published
    -> Element Msg
view dProfile donateChecked showAddressOnPhace blockTimes now wallet state post =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px <| 120
        , Element.Background.color theme.blockBackground
        , Element.Border.width 1
        , Element.Border.color theme.blockBorderColor
        , Element.Border.rounded 5
        , Element.padding 1
        ]
        [ mainPreviewPane
            dProfile
            showAddressOnPhace
            donateChecked
            blockTimes
            now
            (Wallet.unlockStatus wallet)
            --maybeUXModel
            Nothing
            post
        , publishedPostActionForm
            dProfile
            donateChecked
            post
            --(maybeUXModel
            --|> Maybe.andThen .showInput
            --|> Maybe.withDefault None
            --)
            Types.None
            (Wallet.unlockStatus wallet)
        ]


mainPreviewPane :
    DisplayProfile
    -> Bool
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> UnlockStatus
    -> Maybe PostState
    -> Published
    -> Element Msg
mainPreviewPane dProfile showAddress donateChecked blockTimes now unlockStatus maybeUXModel post =
    Element.column
        [ Element.width <| Element.fillPortion 11
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
    -> Published
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
    -> Published
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
    -> Published
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
                        PublishedPost post
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
    -> Context
    -> Element Msg
viewContext dProfile context =
    Element.el
        [ Element.width Element.fill
        , Element.Font.color almostWhite
        ]
    <|
        case context of
            Reply id ->
                Element.text "reply"

            TopLevel topic ->
                Element.text <| "#" ++ topic


viewTiming :
    DisplayProfile
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Id
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
    -> Published
    -> Element Msg
previewBody dProfile showAddress post =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 5
        ]
        --[ View.phaceElement
        --( 60, 60 )
        --True
        --post.core.author
        --showAddress
        --(MsgUp <| Types.ShowOrHideAddress <| PhaceForPublishedPost post.id)
        --NoOp
        [ post.core.content.title
            |> Maybe.withDefault post.core.content.body
            |> limitedString
            |> text
            |> List.singleton
            |> Element.paragraph
                [ Element.Font.color almostWhite
                , Element.Font.size (responsiveVal dProfile 14 8)
                , Element.height Element.fill
                , Element.width Element.fill
                ]
        ]


publishedPostActionForm :
    DisplayProfile
    -> Bool
    -> Published
    -> Types.ShowInputState
    -> UnlockStatus
    -> Element Msg
publishedPostActionForm dProfile donateChecked publishedPost showInput unlockStatus =
    case showInput of
        None ->
            Element.column
                [ Element.spacing 5
                , Element.width <| Element.fillPortion 1
                , Element.alignRight
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
                --(SupportTipSubmitClicked publishedPost.id)
                (always ClickHappened)
                unlockStatus

        Burn input ->
            unlockOrInputForm
                dProfile
                donateChecked
                (Element.rgba 1 0 0 0.1)
                input
                "Burn"
                --(SupportBurnSubmitClicked publishedPost.id)
                (always ClickHappened)
                unlockStatus


supportTipButton :
    Id
    -> Element Msg
supportTipButton postId =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ publishedPostActionButton
            [ EH.withTitle "Tip DAI for this post, rewarding the author"
            , Element.Background.color theme.daiTippedBackground
            ]
            --SupportTipClicked
            ClickHappened
          <|
            Element.image
                [ Element.height <| Element.px 14
                , Element.centerX
                ]
                { src = "img/dai-unit-char-white.svg"
                , description = "support tip"
                }
        ]


supportBurnButton :
    Id
    -> Element Msg
supportBurnButton postId =
    publishedPostActionButton
        [ EH.withTitle "Burn DAI to increase this post's visibility"
        , Element.Background.color theme.daiBurnedBackground
        ]
        --SupportBurnClicked
        ClickHappened
    <|
        Element.image
            [ Element.height <| Element.px 14
            , Element.centerX
            ]
            { src = "img/dai-unit-char-white.svg"
            , description = "support burn"
            }


replyButton :
    Id
    -> Element Msg
replyButton postId =
    publishedPostActionButton
        [ EH.withTitle "Reply"
        , Element.Background.color Theme.blue
        ]
        (Types.StartInlineCompose <| Reply postId)
    <|
        Element.image
            [ Element.width Element.fill
            , Element.height <| Element.px 14
            ]
            { src = "img/reply-arrow-white.svg"
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
               , Element.Border.shadow
                    { offset = ( 0, 0 )
                    , size = 0
                    , blur = 5
                    , color = Element.rgba 0 0 0 0.1
                    }
               , Element.Events.onClick onClick
               , Element.width <| Element.px 20
               , Element.height <| Element.px 20
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
        , Element.width <| Element.fillPortion 1
        ]
        [ unlockUXOr
            dProfile
            []
            unlockStatus
            (inputForm dProfile donateChecked currentString buttonLabel onSubmit)
        , EH.closeButton
            [ Element.alignTop
            , Element.moveUp 5
            , Element.moveRight 5
            ]
            EH.black
            --ResetActionForm
            ClickHappened
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
                currentString
                --AmountInputChanged
                (always ClickHappened)
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
                { onChange = Types.DonationCheckboxSet
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
    150


limitedString : String -> String
limitedString text =
    if String.length text > previewMaxTextLength then
        text
            ++ "..."
            |> String.replace "\n" " "
            |> String.slice 0 previewMaxTextLength

    else
        text
