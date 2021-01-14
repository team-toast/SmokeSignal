module TopicUX.View exposing (..)

import Color
import Common.Types exposing (..)
import Common.View exposing (daiAmountInput, daiSymbol, unlockUXOr, whiteGlowAttribute)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element, alignRight, alignTop, column, el, fill, fillPortion, height, padding, px, row, spacing, text, width)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
import Helpers.Time as TimeHelpers
import Html.Attributes
import Post
import Theme exposing (almostWhite, lightGray, theme)
import Time
import TokenValue exposing (TokenValue)
import TopicUX.Types exposing (..)
import Wallet


topicHeader :
    DisplayProfile
    -> String
    -> Element Msg
topicHeader dProfile topic =
    column
        [ whiteGlowAttribute
        , Element.Font.color EH.white
        , width fill
        , padding 10
        ]
        [ text topic ]


view :
    DisplayProfile
    -> Bool
    -> Bool
    -> String
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Wallet
    -> Maybe ShowInputState
    -> PublishedPostsDict
    -> Element Msg
view dProfile donateChecked showAddressOnPhace topic blockTimes now wallet inputState posts =
    let
        listOfPosts =
            let
                isTopicMatch : String -> Published -> Bool
                isTopicMatch topicToFind post =
                    case post.core.metadata.context of
                        TopLevel postTopic ->
                            postTopic == topicToFind

                        Reply postId ->
                            False
            in
            posts
                |> Dict.values
                |> List.concat
                |> List.filter (isTopicMatch topic)
    in
    column
        [ width fill
        , height <| px <| 120
        , Element.Background.color theme.blockBackground
        , Element.Border.width 1
        , Element.Border.color theme.blockBorderColor
        , Element.Border.rounded 5
        , padding 1
        ]
    <|
        List.map
            (\post ->
                mainPreviewPane
                    dProfile
                    showAddressOnPhace
                    donateChecked
                    blockTimes
                    now
                    (Wallet.unlockStatus wallet)
                    (Maybe.withDefault None inputState)
                    post
            )
            listOfPosts


mainPreviewPane :
    DisplayProfile
    -> Bool
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> UnlockStatus
    -> ShowInputState
    -> Published
    -> Element Msg
mainPreviewPane dProfile showAddress donateChecked blockTimes now unlockStatus inputState post =
    row []
        [ column
            [ width <| fillPortion 11
            , height fill
            , spacing 10
            ]
            [ previewMetadata dProfile blockTimes now post
            , previewBody dProfile showAddress post
            ]
        , publishedPostActionForm
            dProfile
            donateChecked
            post
            inputState
            unlockStatus
        ]


previewMetadata :
    DisplayProfile
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Published
    -> Element Msg
previewMetadata dProfile blockTimes now post =
    row
        [ Element.Font.size <| responsiveVal dProfile 16 10
        , width <| px 300
        , spacing 40
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
    row
        [ width <| px 100
        , spacing 5
        ]
        [ viewDaiBurned dProfile post
        , Maybe.map (viewDaiTipped dProfile)
            (post.maybeAccounting |> Maybe.map .totalTipped)
            |> Maybe.withDefault Element.none
        ]


commonDaiElStyles : List (Element.Attribute Msg)
commonDaiElStyles =
    [ spacing 3
    , padding 3
    , Element.Border.rounded 3
    , Element.Font.size 14
    ]


viewDaiBurned :
    DisplayProfile
    -> Published
    -> Element Msg
viewDaiBurned dProfile post =
    row
        (commonDaiElStyles
            ++ [ Element.Background.color theme.daiBurnedBackground ]
        )
        [ daiSymbol True [ height <| px 14 ]
        , el
            [ Element.Font.color EH.white ]
          <|
            text <|
                TokenValue.toConciseString <|
                    Post.totalBurned <|
                        PublishedPost post
        ]


viewDaiTipped :
    DisplayProfile
    -> TokenValue
    -> Element Msg
viewDaiTipped dProfile amount =
    row
        (commonDaiElStyles
            ++ [ Element.Background.color theme.daiTippedBackground ]
        )
        [ daiSymbol True [ height <| px 14 ]
        , el
            [ Element.Font.color EH.white ]
          <|
            text <|
                TokenValue.toConciseString amount
        ]


viewContext :
    DisplayProfile
    -> Context
    -> Element Msg
viewContext dProfile context =
    (case context of
        Reply id ->
            text "reply"

        TopLevel topic ->
            text <| "#" ++ topic
    )
        |> el
            [ width fill
            , Element.Font.color almostWhite
            ]


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
    text
        (maybeTimePassed
            |> Maybe.map TimeHelpers.roundToSingleUnit
            |> Maybe.map (\s -> s ++ " ago")
            |> Maybe.withDefault "..."
        )
        |> el
            [ width <| px 100
            , Element.Font.color theme.subtleTextColor
            ]


previewBody :
    DisplayProfile
    -> Bool
    -> Published
    -> Element Msg
previewBody dProfile showAddress post =
    [ Common.View.phaceElement
        ( 60, 60 )
        True
        post.core.author
        showAddress
        (MsgUp <| Common.Types.ShowOrHideAddress <| PhaceForPublishedPost post.id)
        NoOp
    , viewTitleOrTextPreview dProfile post.core.content
    ]
        |> row
            [ width fill
            , height fill
            , spacing 5
            ]


viewTitleOrTextPreview :
    DisplayProfile
    -> Content
    -> Element Msg
viewTitleOrTextPreview dProfile content =
    [ text <|
        limitedString <|
            case content.title of
                Just title ->
                    title

                Nothing ->
                    content.body
    ]
        |> row
            [ Element.Font.color almostWhite
            , Element.Font.size (responsiveVal dProfile 14 8)
            , height fill
            , width fill
            ]


publishedPostActionForm :
    DisplayProfile
    -> Bool
    -> Published
    -> ShowInputState
    -> UnlockStatus
    -> Element Msg
publishedPostActionForm dProfile donateChecked publishedPost showInput unlockStatus =
    case showInput of
        None ->
            [ supportTipButton publishedPost.id
            , supportBurnButton publishedPost.id
            , replyButton publishedPost.id
            ]
                |> column
                    [ spacing 5
                    , width <| fillPortion 1
                    , alignRight
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
    Id
    -> Element Msg
supportTipButton postId =
    [ publishedPostActionButton
        [ EH.withTitle "Tip DAI for this post, rewarding the author"
        , Element.Background.color theme.daiTippedBackground
        ]
        SupportTipClicked
      <|
        Element.image
            [ height <| px 14
            , Element.centerX
            ]
            { src = "img/dai-unit-char-white.svg"
            , description = "support tip"
            }
    ]
        |> row
            [ width fill
            , height fill
            ]


supportBurnButton :
    Id
    -> Element Msg
supportBurnButton postId =
    publishedPostActionButton
        [ EH.withTitle "Burn DAI to increase this post's visibility"
        , Element.Background.color theme.daiBurnedBackground
        ]
        SupportBurnClicked
    <|
        Element.image
            [ height <| px 14
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
        (MsgUp <| Common.Types.StartInlineCompose <| Reply postId)
    <|
        Element.image
            [ width fill
            , height <| px 14
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
    el
        (attributes
            ++ [ padding 3
               , Element.pointer
               , Element.Border.rounded 1
               , Element.Border.shadow
                    { offset = ( 0, 0 )
                    , size = 0
                    , blur = 5
                    , color = Element.rgba 0 0 0 0.1
                    }
               , Element.Events.onClick onClick
               , width <| px 20
               , height <| px 20
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
    [ unlockUXOr
        dProfile
        []
        unlockStatus
        MsgUp
        (inputForm dProfile donateChecked currentString buttonLabel onSubmit)
    , EH.closeButton
        [ alignTop
        , Element.moveUp 5
        , Element.moveRight 5
        ]
        EH.black
        ResetActionForm
    ]
        |> row
            [ padding 10
            , Element.Border.rounded 6
            , Element.Background.color bgColor
            , spacing 10
            , Element.Border.glow
                (Element.rgba 0 0 0 0.1)
                5
            , width <| fillPortion 1
            ]


inputForm :
    DisplayProfile
    -> Bool
    -> String
    -> String
    -> (TokenValue -> Msg)
    -> Element Msg
inputForm dProfile donateChecked currentString buttonLabel onSubmit =
    [ [ daiSymbol False
            [ height <| px 22 ]
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
        |> row
            [ spacing 10
            , Element.centerX
            ]
    , [ Element.Input.checkbox
            []
            { onChange = MsgUp << Common.Types.DonationCheckboxSet
            , icon = Element.Input.defaultCheckbox
            , checked = donateChecked
            , label =
                Element.Input.labelRight
                    [ Element.centerY
                    ]
                <|
                    text "Donate an extra 1% to "
            }
      , Element.newTabLink
            [ Element.Font.color theme.linkTextColor
            , Element.centerY
            ]
            { url = "https://foundrydao.com/"
            , label = text "Foundry"
            }
      ]
        |> row
            [ Element.centerX
            , Element.Font.size 12
            ]
    ]
        |> column
            [ spacing 10 ]


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
