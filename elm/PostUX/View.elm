module PostUX.View exposing (view)

--import PostUX.Types exposing (Msg(..))

import Common.Types exposing (..)
import Common.View exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Lazy
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Eth as EthHelpers
import List.Extra
import Maybe.Extra
import Post
import Routing
import Theme exposing (theme)
import Time
import TokenValue exposing (TokenValue)
import Wallet


view : DisplayProfile -> Bool -> Bool -> Post -> Wallet -> Maybe Model -> Element Msg
view dProfile donateChecked showContext post wallet maybeUXModel =
    let
        postCore =
            Post.getCore post

        maybePostId =
            case post of
                PublishedPost publishedPost ->
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
                        ( 100, 100 )
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
                [ Element.width Element.fill
                , Element.spacing 5
                ]
                [ viewDaiBurned post
                , if Post.totalTipped post |> TokenValue.isZero then
                    Element.none

                  else
                    viewDaiTipped post
                , Maybe.map viewPostLinks maybePostId
                    |> Maybe.withDefault Element.none
                ]
            , viewMainPostBlock
                dProfile
                donateChecked
                showContext
                post
                (Wallet.unlockStatus wallet)
                maybeUXModel
            ]
        ]


makePhaceElement : ( Int, Int ) -> Address -> Maybe Model -> Element Msg
makePhaceElement ( width, height ) author maybeUXModel =
    --phaceElement
    --( width, height )
    --True
    --author
    --(maybeUXModel
    --|> Maybe.map .showAddress
    --|> Maybe.withDefault False
    --)
    --PhaceIconClicked
    --NoOp
    Element.none


viewDaiBurned : Post -> Element Msg
viewDaiBurned post =
    viewDaiStatTab
        theme.daiBurnedBackground
        theme.daiBurnedTextIsWhite
        (burnSummaryString post)
        (Post.totalBurned post)


viewDaiTipped : Post -> Element Msg
viewDaiTipped post =
    viewDaiStatTab
        theme.daiTippedBackground
        theme.daiTippedTextIsWhite
        (tipSummaryString post)
        (Post.totalTipped post)


viewDaiStatTab : Element.Color -> Bool -> String -> TokenValue -> Element Msg
viewDaiStatTab bgColor textIsWhite mouseoverTitle amount =
    Element.el
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.Background.color bgColor
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignLeft
        , EH.withTitle mouseoverTitle
        ]
    <|
        Element.row
            [ Element.spacing 3
            ]
            [ daiSymbol textIsWhite [ Element.height <| Element.px 18 ]
            , Element.text <| (amount |> TokenValue.toConciseString)
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
                ", Crowd burned $"
                    ++ (crowdBurned |> TokenValue.toConciseString)
                    ++ " in support"
           )


tipSummaryString : Post -> String
tipSummaryString post =
    "Author received $"
        ++ (Post.totalTipped post |> TokenValue.toConciseString)
        ++ " in tips"


viewPostLinks : Id -> Element Msg
viewPostLinks postId =
    let
        route =
            RouteViewContext <|
                ViewPost postId
    in
    Element.row
        [ Element.alignBottom
        , Element.paddingXY 10 5
        , Element.Font.size 20
        , Element.Background.color theme.postBodyBackground
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
            [ Element.Font.color theme.linkTextColor
            , Element.pointer
            , Element.Font.bold
            , Element.Events.onClick <|
                MsgUp <|
                    Common.Types.GotoRoute <|
                        route
            ]
            (Element.text (shortenedHash postId.messageHash))
        , Element.newTabLink
            [ Element.Font.color theme.linkTextColor
            , Element.Font.size 16
            ]
            { url =
                Routing.routeToFullDotEthUrlString <|
                    RouteViewContext <|
                        ViewPost postId
            , label = Element.text "(.eth permalink)"
            }
        ]


viewMainPostBlock : DisplayProfile -> Bool -> Bool -> Post -> UnlockStatus -> Maybe Model -> Element Msg
viewMainPostBlock dProfile donateChecked showContext post unlockStatus maybeUXModel =
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
                        ( 100, 100 )
                        postCore.author
                        maybeUXModel
            , Element.map MsgUp <| viewMetadata showContext postCore.metadata
            ]
        , Element.map never postCore.renderedPost
        , case post of
            PublishedPost published ->
                --publishedPostActionForm
                --dProfile
                --donateChecked
                --published
                --(maybeUXModel
                --|> Maybe.map .showInput
                --|> Maybe.withDefault None
                --)
                --unlockStatus
                Element.none

            _ ->
                Element.none
        ]


publishedPostActionForm : DisplayProfile -> Bool -> Published -> ShowInputState -> UnlockStatus -> Element Msg
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


supportTipButton : Id -> Element Msg
supportTipButton postId =
    publishedPostActionButton
        [ EH.withTitle "Tip DAI for this post, rewarding the author" ]
        SupportTipClicked
    <|
        Element.image
            [ Element.height <| Element.px 18
            , Element.centerX
            ]
            { src = "img/dai-unit-char-green.svg"
            , description = "support green"
            }


supportBurnButton : Id -> Element Msg
supportBurnButton postId =
    publishedPostActionButton
        [ EH.withTitle "Burn DAI to increase this post's visibility" ]
        SupportBurnClicked
    <|
        Element.image
            [ Element.height <| Element.px 18
            , Element.centerX
            ]
            { src = "img/dai-unit-char-red.svg"
            , description = "support burn"
            }


replyButton : Id -> Element Msg
replyButton postId =
    publishedPostActionButton
        [ EH.withTitle "Reply" ]
        (MsgUp <| Common.Types.StartInlineCompose <| Reply postId)
    <|
        Element.image
            [ Element.width Element.fill ]
            { src = "img/reply-arrow.svg"
            , description = "reply"
            }


publishedPostActionButton : List (Attribute Msg) -> Msg -> Element Msg -> Element Msg
publishedPostActionButton attributes onClick innerEl =
    Element.el
        (attributes
            ++ [ Element.padding 7
               , Element.pointer
               , Element.Border.rounded 4
               , Element.Background.color <| Element.rgba 1 1 1 0.3
               , Element.Border.shadow
                    { offset = ( 0, 0 )
                    , size = 0
                    , blur = 5
                    , color = Element.rgba 0 0 0 0.1
                    }
               , Element.Events.onClick onClick
               , Element.width <| Element.px 30
               , Element.height <| Element.px 30
               ]
        )
        innerEl


unlockOrInputForm : DisplayProfile -> Bool -> Element.Color -> String -> String -> (TokenValue -> Msg) -> UnlockStatus -> Element Msg
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


inputForm : DisplayProfile -> Bool -> String -> String -> (TokenValue -> Msg) -> Element Msg
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
                { onChange = MsgUp << Common.Types.DonationCheckboxSet
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


maybeSubmitButton : DisplayProfile -> String -> Maybe TokenValue -> (TokenValue -> Msg) -> Element Msg
maybeSubmitButton dProfile label maybeAmount onSubmit =
    case maybeAmount of
        Just amount ->
            theme.emphasizedActionButton
                Mobile
                []
                [ label ]
                (EH.Action <| onSubmit amount)

        Nothing ->
            Theme.disabledButton
                Mobile
                []
                label
