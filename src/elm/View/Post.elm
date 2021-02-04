module View.Post exposing (view)

import Dict exposing (Dict)
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, padding, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile, black, responsiveVal, white)
import Helpers.Time as TimeHelpers
import Misc
import Set
import Theme exposing (almostWhite, theme)
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, whiteGlowAttributeSmall)
import View.Common exposing (daiAmountInput, daiSymbol, phaceElement, whenJust)
import View.Img
import View.Markdown


view :
    DisplayProfile
    -> Maybe Posix
    -> Posix
    -> Set.Set Types.PostKey
    -> Maybe Accounting
    -> String
    -> CoreData
    -> Element Msg
view dProfile timestamp now replies accounting title post =
    [ [ accounting
            |> whenJust (viewAccounting dProfile)
      , [ title
            |> text
            |> el [ Font.size 30 ]
        , [ text <| "Block " ++ String.fromInt post.id.block
          , viewTiming dProfile timestamp now post.id

          --, viewContext dProfile post.core.metadata.context
          ]
            |> row
                [ spacing 20
                , Font.size 17
                , Font.color theme.subtleTextColor
                ]
        ]
            |> row [ width fill, spaceEvenly ]
      ]
        |> row [ width fill, spacing 10 ]
    , [ phaceElement
            ( 60, 60 )
            False
            post.author
            False
            ClickHappened
      , [ viewContent post
        , [ [ ( viewReplies replies, View.Img.speechBubble )

            --, ( "Bookmark", View.Img.bookmark )
            --, ( "Hide", View.Img.hide )
            ]
                |> List.map
                    (\( txt, icn ) ->
                        [ icn 17 almostWhite
                        , text txt
                        ]
                            |> row [ spacing 10, Font.size 23 ]
                    )
                |> row [ spacing 10, Font.color almostWhite, Font.size 17 ]
          , [ supportTipButton post.id
            , supportBurnButton post.id
            , replyButton post.id
            ]
                |> row [ spacing 10 ]
          ]
            |> row
                [ spacing 10
                , Element.alignRight
                ]
        ]
            |> column
                [ spacing 10
                , width fill
                ]
      ]
        |> row [ width fill, spacing 10 ]
    ]
        |> column
            [ width fill
            , spacing 5
            ]
        |> el
            [ Background.color black
            , Font.color white
            , whiteGlowAttributeSmall
            , padding 5
            , width fill
            ]


viewContent : CoreData -> Element Msg
viewContent post =
    Input.button [ width fill ]
        { onPress = Just <| GotoView <| ViewPost post.id
        , label =
            [ post.content.title |> whenJust (text >> el [ Font.bold ])
            , post.content.desc |> whenJust (text >> el [ Font.italic ])
            , post.content.body
                |> View.Markdown.renderString
                    [ height <| px 100
                    , Element.clip
                    ]
                |> Result.toMaybe
                |> whenJust identity
            ]
                |> column
                    [ width fill
                    , View.Attrs.sansSerifFont
                    , spacing 10
                    ]
        }


viewReplies : Set.Set a -> String
viewReplies replies =
    let
        len =
            Set.size replies

        word =
            if len == 1 then
                "reply"

            else
                "replies"
    in
    String.fromInt len ++ " " ++ word


mainPreviewPane :
    DisplayProfile
    -> Bool
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe PostState
    -> Published
    -> Element Msg
mainPreviewPane dProfile showAddress donateChecked blockTimes now maybeUXModel post =
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
        [ Font.size <| responsiveVal dProfile 16 10
        , Element.width <| Element.px 300
        , Element.spacing 40
        ]
        [ -- viewAccounting dProfile post
          --viewTiming dProfile blockTimes now post.id
          viewContext dProfile post.core.metadata.context
        ]


viewAccounting : DisplayProfile -> Accounting -> Element Msg
viewAccounting dProfile accounting =
    Element.row
        [ spacing 5
        ]
        [ -- viewDaiBurned dProfile post
          viewDaiTipped dProfile accounting.totalTipped
        ]


commonDaiElStyles : List (Element.Attribute Msg)
commonDaiElStyles =
    [ Element.spacing 3
    , Element.padding 3
    , Element.Border.rounded 3
    , Font.size 14
    ]


viewDaiBurned :
    DisplayProfile
    -> RootPost
    -> Element Msg
viewDaiBurned dProfile post =
    Element.row
        (commonDaiElStyles
            ++ [ Background.color theme.daiBurnedBackground ]
        )
        [ daiSymbol True [ Element.height <| Element.px 14 ]

        --, Element.el
        --[ Font.color EH.white ]
        --<|
        --Element.text <|
        --TokenValue.toConciseString <|
        --Misc.totalBurned <|
        --PublishedPost post
        ]


viewDaiTipped :
    DisplayProfile
    -> TokenValue
    -> Element Msg
viewDaiTipped dProfile amount =
    Element.row
        (commonDaiElStyles
            ++ [ Background.color theme.daiTippedBackground ]
        )
        [ daiSymbol True [ Element.height <| Element.px 14 ]
        , Element.el
            [ Font.color EH.white ]
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
        , Font.color almostWhite
        ]
    <|
        case context of
            Reply id ->
                Element.text "reply"

            TopLevel topic ->
                Element.text <| "#" ++ topic


viewTiming :
    DisplayProfile
    -> Maybe Time.Posix
    -> Time.Posix
    -> PostId
    -> Element Msg
viewTiming dProfile maybePostTime now id =
    let
        maybeTimePassed =
            maybePostTime
                |> Maybe.map
                    (\postTime ->
                        TimeHelpers.sub now postTime
                    )
    in
    maybePostTime
        |> whenJust
            (\time ->
                [ text <| Misc.formatPosix time
                , TimeHelpers.sub now time
                    |> TimeHelpers.roundToSingleUnit
                    |> (\s -> s ++ " ago")
                    --|> Maybe.withDefault "..."
                    |> text
                    |> el
                        [ Font.color theme.subtleTextColor
                        ]

                --, maybeTimePassed
                --|> Maybe.map TimeHelpers.roundToSingleUnit
                --|> Maybe.map (\s -> s ++ " ago")
                --|> Maybe.withDefault "..."
                --|> text
                --|> el
                --[ Font.color theme.subtleTextColor
                --]
                ]
                    |> row [ spacing 20 ]
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
                [ Font.color almostWhite
                , Font.size (responsiveVal dProfile 14 8)
                , Element.height Element.fill
                , Element.width Element.fill
                ]
        ]


publishedPostActionForm :
    DisplayProfile
    -> Bool
    -> Published
    -> Types.ShowInputState
    -> Element Msg
publishedPostActionForm dProfile donateChecked publishedPost showInput =
    case showInput of
        None ->
            Element.column
                [ Element.spacing 5
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

        Burn input ->
            unlockOrInputForm
                dProfile
                donateChecked
                (Element.rgba 1 0 0 0.1)
                input
                "Burn"
                --(SupportBurnSubmitClicked publishedPost.id)
                (always ClickHappened)


supportTipButton :
    PostId
    -> Element Msg
supportTipButton postId =
    Input.button
        [ height <| px 40
        , Background.color theme.daiTippedBackground
        , width <| px 40
        , EH.withTitle "Tip DAI for this post, rewarding the author"
        , hover
        ]
        { onPress = Just <| SetTipOpen postId
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }


supportBurnButton :
    PostId
    -> Element Msg
supportBurnButton postId =
    publishedPostActionButton
        [ EH.withTitle "Burn DAI to increase this post's visibility"
        , Background.color theme.daiBurnedBackground
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


replyButton : PostId -> Element Msg
replyButton postId =
    View.Img.replyArrow 14 white
        |> publishedPostActionButton
            [ EH.withTitle "Reply"
            , Background.color Theme.blue
            ]
            (Types.StartInlineCompose <| Reply postId)


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
    -> Element Msg
unlockOrInputForm dProfile donateChecked bgColor currentString buttonLabel onSubmit =
    Element.row
        [ Element.padding 10
        , Element.Border.rounded 6
        , Background.color bgColor
        , Element.spacing 10
        , Element.Border.glow
            (Element.rgba 0 0 0 0.1)
            5
        , Element.width <| Element.fillPortion 1
        ]
        [ EH.closeButton
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
            , Font.size 12
            ]
            [ Input.checkbox
                []
                { onChange = Types.DonationCheckboxSet
                , icon = Input.defaultCheckbox
                , checked = donateChecked
                , label =
                    Input.labelRight
                        [ Element.centerY
                        ]
                    <|
                        Element.text "Donate an extra 1% to "
                }
            , Element.newTabLink
                [ Font.color theme.linkTextColor
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
