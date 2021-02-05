module View.Post exposing (view)

import Dict exposing (Dict)
import Element exposing (Attribute, Color, Element, centerX, centerY, column, el, fill, height, padding, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile, black, responsiveVal, white)
import Helpers.Time as TimeHelpers
import Maybe.Extra
import Misc
import Set exposing (Set)
import Theme exposing (almostWhite, theme)
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, typeFont, whiteGlowAttributeSmall)
import View.Common exposing (daiAmountInput, daiSymbol, phaceElement, whenJust)
import View.Img
import View.Markdown


view :
    DisplayProfile
    -> Maybe Posix
    -> Posix
    -> Set.Set Types.PostKey
    -> Maybe Accounting
    -> Maybe ShowInputState
    -> Float
    -> String
    -> String
    -> CoreData
    -> Element Msg
view dProfile timestamp now replies accounting state ethPrice input title post =
    [ [ accounting
            |> whenJust (viewAccounting dProfile ethPrice)
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
            |> linkToPost post.id
        , [ state
                |> Maybe.Extra.unwrap
                    (viewActions replies post)
                    (viewInput dProfile post input)
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
            , spacing 10
            ]
        |> el
            [ Background.color black
            , Font.color white
            , whiteGlowAttributeSmall
            , padding 10
            , width fill
            , typeFont
            ]


linkToPost : PostId -> Element Msg -> Element Msg
linkToPost id elem =
    Input.button [ width fill, hover ]
        { onPress = Just <| GotoView <| ViewPost id
        , label = elem
        }


viewContent : CoreData -> Element Msg
viewContent post =
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


viewActions : Set PostKey -> CoreData -> Element Msg
viewActions replies post =
    [ [ ( viewReplies replies, View.Img.speechBubble )

      --, ( "Bookmark", View.Img.bookmark )
      --, ( "Hide", View.Img.hide )
      ]
        |> List.map
            (\( txt, icn ) ->
                [ icn 17 almostWhite
                , text txt
                ]
                    |> row [ spacing 10, Font.size 23 ]
                    |> linkToPost post.id
            )
        |> row [ spacing 10, Font.color almostWhite, Font.size 17 ]
    , supportTipButton post.id
    , supportBurnButton post.id

    --, replyButton post.id
    ]
        |> row [ spacing 10 ]


viewReplies : Set a -> String
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
    -> Maybe Int
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


viewAccounting : DisplayProfile -> Float -> Accounting -> Element Msg
viewAccounting dProfile ethPrice accounting =
    Element.row
        [ spacing 5
        ]
        [ viewAmount theme.daiTippedBackground accounting.totalTipped ethPrice
        , viewAmount theme.daiBurnedBackground accounting.totalBurned ethPrice
        ]


viewAmount : Color -> TokenValue -> Float -> Element Msg
viewAmount color amount ethPrice =
    [ View.Img.dollar 22 white
    , Misc.tokenToDollar ethPrice amount
        |> text
    ]
        |> row
            [ Element.padding 3
            , Element.Border.rounded 3
            , Font.size 22
            , Font.color white
            , Background.color color
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


viewInput : DisplayProfile -> CoreData -> String -> ShowInputState -> Element Msg
viewInput dProfile post input showInput =
    let
        title =
            case showInput of
                Tip _ ->
                    "Tip Ether for this post, rewarding the author."

                Burn _ ->
                    "Burn Ether to increase the visibility of this post."

        msg =
            case showInput of
                Tip _ ->
                    SubmitTip post.id

                Burn _ ->
                    --"Burn DAI to increase this post's visibility"
                    SubmitBurn post.id
    in
    [ text title
    , Input.text [ Font.color black ]
        { onChange = ComposeDaiChange
        , label = Input.labelHidden ""
        , placeholder =
            "0.0"
                |> text
                |> Input.placeholder []
                |> Just
        , text = input
        }
    , [ Input.button
            [ Font.underline
            , hover
            ]
            { onPress = Just CancelTipOpen
            , label = text "Cancel"
            }
      , Input.button
            [ Background.color Theme.orange
            , padding 10
            , roundBorder
            , hover
            , Font.color black
            ]
            { onPress = Just msg
            , label = text "Submit"
            }
      ]
        |> row [ Element.alignRight, spacing 20 ]
    ]
        |> column
            [ Background.color black
            , spacing 20
            , padding 20
            , width fill
            , Font.color white
            , View.Attrs.sansSerifFont
            ]


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
        { onPress = Just <| SetTipOpen <| PostState postId (Types.Tip "")
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }


supportBurnButton :
    PostId
    -> Element Msg
supportBurnButton postId =
    Input.button
        [ height <| px 40
        , Background.color theme.daiBurnedBackground
        , width <| px 40
        , EH.withTitle "Burn DAI to increase this post's visibility"
        , hover
        ]
        { onPress = Just <| SetTipOpen <| PostState postId (Types.Burn "")
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
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
