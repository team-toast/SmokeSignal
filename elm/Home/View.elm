module Home.View exposing (view)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Utils
import Helpers.Element as EH exposing (changeForMobile)
import Home.Types exposing (..)
import Post exposing (Post)
import Routing exposing (Route)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view : EH.DisplayProfile -> Model -> WalletUXPhaceInfo -> Dict Int (List Post) -> Element Msg
view dProfile model walletUXPhaceInfo posts =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color darkTheme.appBackground
        , Element.paddingXY 20 40
            |> changeForMobile (Element.paddingXY 10 20) dProfile
        , Element.spacing (60 |> changeForMobile 15 dProfile)
        , Element.Font.color darkTheme.emphasizedTextColor
        ]
        [ Element.column
            [ Element.centerX
            , Element.spacing 20
            ]
          <|
            [ Element.paragraph
                [ Element.Font.size 50
                , Element.Font.center
                ]
                [ Element.text "Freedom of Speech is being attacked." ]
            , Element.paragraph
                [ Element.Font.size 60
                , Element.Font.center
                , Element.Font.semiBold
                ]
                [ Element.text "SmokeSignal makes this futile." ]
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 20
            ]
            [ Element.el
                [ Element.width (Element.fillPortion 1)
                , Element.alignTop
                ]
              <|
                Element.none
            , Element.column
                [ Element.width (Element.fillPortion 2)
                , Element.alignTop
                , Element.spacing 40
                ]
                [ composeActionBlock dProfile walletUXPhaceInfo
                , infoBlock
                ]
            , Element.el
                [ Element.width (Element.fillPortion 1)
                , Element.alignTop
                ]
              <|
                topicsBlock dProfile model posts
            ]
        ]


infoBlock : Element Msg
infoBlock =
    Element.column
        [ Element.Border.rounded 15
        , Element.Background.color darkTheme.blockBackground
        , Element.padding 25
        , Element.Font.color <| EH.white
        , Element.Font.size 26
        , Element.Font.color darkTheme.mainTextColor
        , Element.centerX
        , Element.spacing 20
        ]
    <|
        List.map
            (Element.paragraph
                [ Element.width Element.fill
                , Element.Font.center
                ]
            )
            [ [ Element.text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat." ]
            , [ emphasizedText "No usernames. No moderators. No deplatforming."
              ]
            , [ Element.text "All you need is ETH for gas and DAI to burn." ]
            ]


composeActionBlock : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
composeActionBlock dProfile walletUXPhaceInfo =
    let
        paragrapher paras =
            Element.column
                [ Element.spacing 15 ]
                (List.map
                    (Element.paragraph
                        [ Element.Font.size 24
                        , Element.width Element.fill
                        , Element.Font.color darkTheme.mainTextColor
                        ]
                    )
                    paras
                )
    in
    Element.column
        [ Element.spacing 25
        , Element.centerX
        , Element.width <| Element.px 600
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 40
            ]
            [ homeWalletUX dProfile <|
                walletUXPhaceInfo
            , Element.column
                [ Element.spacing 5
                , Element.Font.size 50
                , Element.Font.bold
                , Element.alignTop
                ]
                (case walletUXPhaceInfo of
                    UserPhaceInfo _ ->
                        [ Element.text "That's your Phace!"
                        , Element.text "What a cutie."
                        ]

                    DemoPhaceInfo _ ->
                        [ Element.text "Don your Phace."
                        , Element.text "Have your say."
                        ]
                )
            ]
        , paragrapher <|
            case walletUXPhaceInfo of
                UserPhaceInfo _ ->
                    [ [ Element.text "If you don't like that Phace, try switching accounts in your wallet." ]
                    , [ Element.text "Otherwise, you're now free to cavort all over SmokeSignal and wreak all sorts of "
                      , emphasizedText "immutable havock."
                      ]
                    ]

                DemoPhaceInfo _ ->
                    [ [ Element.text "Your Ethereum address maps to a unique Phace, which will be shown next to any SmokeSignal posts you write." ]
                    , [ Element.text "Connect your Web3 Wallet to see your Phace." ]
                    ]
        , case walletUXPhaceInfo of
            DemoPhaceInfo _ ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ Element.map MsgUp <|
                        web3ConnectButton dProfile [ Element.width Element.fill ]
                    ]

            _ ->
                Element.none
        ]


homeWalletUX : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
homeWalletUX dProfile walletUXPhaceInfo =
    Element.map MsgUp <|
        case walletUXPhaceInfo of
            DemoPhaceInfo demoAddress ->
                Element.column
                    [ Element.spacing 5
                    , Element.pointer
                    , Element.Events.onClick <| ConnectToWeb3
                    , Element.Border.rounded 10
                    , Element.Border.glow
                        (Element.rgba 1 0 1 0.3)
                        9
                    ]
                    [ phaceElement
                        True
                        MorphingPhace
                        (Eth.Utils.unsafeToAddress demoAddress)
                        False
                    ]

            -- Element.el commonAttributes <|
            UserPhaceInfo ( accountInfo, showAddress ) ->
                Element.el [] <|
                    phaceElement
                        True
                        UserPhace
                        accountInfo.address
                        showAddress


topicsBlock : EH.DisplayProfile -> Model -> Dict Int (List Post) -> Element Msg
topicsBlock dProfile model posts =
    Element.column
        [ Element.spacing 25
        , Element.alignTop
        , Element.width Element.fill
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.alignTop
            ]
            [ Element.Input.text
                [ Element.width Element.fill
                , Element.Background.color <| Element.rgba 1 1 1 0.2
                , Element.Border.color <| Element.rgba 1 1 1 0.6
                ]
                { onChange = TopicInputChanged
                , text = model.topicInput
                , placeholder =
                    Just <|
                        Element.Input.placeholder
                            [ Element.Font.color <| Element.rgba 1 1 1 0.4
                            , Element.Font.italic
                            ]
                            (Element.text "Find or Create Topic")
                , label = Element.Input.labelHidden "topic"
                }
            , topicsColumn dProfile model.topicInput posts
            ]
        ]


topicsColumn : EH.DisplayProfile -> String -> Dict Int (List Post) -> Element Msg
topicsColumn dProfile topicSearchStr posts =
    let
        talliedTopics : List ( String, ( TokenValue, Int ) )
        talliedTopics =
            posts
                |> Dict.values
                |> List.concat
                |> Dict.Extra.groupBy
                    (.metadata
                        >> Result.toMaybe
                        >> Maybe.map .topic
                        >> Maybe.withDefault Post.defaultTopic
                    )
                |> Dict.map
                    (\topic messages ->
                        ( List.foldl (.burnAmount >> TokenValue.add) TokenValue.zero messages
                        , List.length messages
                        )
                    )
                |> Dict.toList
                |> List.sortBy (Tuple.second >> Tuple.first >> TokenValue.toFloatWithWarning >> negate)

        filteredTalliedTopics =
            talliedTopics
                |> List.filter
                    (\( topic, _ ) ->
                        String.contains topicSearchStr topic
                    )

        commonElStyles =
            [ Element.spacing 5
            , Element.padding 5
            , Element.Border.width 1
            , Element.Border.color <| Element.rgba 1 1 1 0.3
            , Element.width Element.fill
            , Element.pointer
            , Element.height <| Element.px 40
            ]

        topicEls =
            filteredTalliedTopics
                |> List.map
                    (\( topic, ( totalBurned, count ) ) ->
                        Element.row
                            (commonElStyles
                                ++ [ Element.Background.color <| Element.rgba 0 0 1 0.2
                                   , Element.Events.onClick <|
                                        GotoRoute <|
                                            Routing.ViewTopic topic
                                   ]
                            )
                            [ Element.row
                                [ Element.padding 5
                                , Element.spacing 3
                                , Element.Border.rounded 5
                                , Element.Background.color darkTheme.daiBurnedBackground
                                , Element.Font.color
                                    (if darkTheme.daiBurnedTextIsWhite then
                                        EH.white

                                     else
                                        EH.black
                                    )
                                ]
                                [ daiSymbol darkTheme.daiBurnedTextIsWhite [ Element.height <| Element.px 18 ]
                                , Element.text <| TokenValue.toConciseString totalBurned
                                ]
                            , Element.el [ Element.centerX ] <| Element.text topic
                            , Element.el [ Element.alignRight ] <| Element.text <| String.fromInt count
                            ]
                    )

        exactTopicFound =
            talliedTopics
                |> List.any (Tuple.first >> (==) topicSearchStr)

        maybeCreateTopicEl =
            if topicSearchStr /= "" && not exactTopicFound then
                Just <|
                    Element.el
                        (commonElStyles
                            ++ [ Element.Background.color <| Element.rgba 0.5 0.5 1 0.4
                               , Element.Events.onClick <| GotoRoute <| Routing.Compose topicSearchStr
                               ]
                        )
                    <|
                        Element.row
                            [ Element.centerX
                            , Element.centerY
                            ]
                            [ Element.text "Start new topic "
                            , Element.el
                                [ Element.Font.italic
                                , Element.Font.bold
                                , Element.Font.color EH.white
                                ]
                              <|
                                Element.text topicSearchStr
                            ]

            else
                Nothing
    in
    Element.map MsgUp <|
        Element.column
            [ Element.Border.roundEach
                { topRight = 0
                , topLeft = 0
                , bottomRight = 5
                , bottomLeft = 5
                }
            , Element.width Element.fill
            , Element.height <| Element.px 300
            , Element.scrollbarY
            , Element.Background.color <| Element.rgba 1 1 1 0.2
            , Element.padding 5
            , Element.spacing 5
            ]
            ((Maybe.map List.singleton maybeCreateTopicEl
                |> Maybe.withDefault []
             )
                ++ topicEls
            )
