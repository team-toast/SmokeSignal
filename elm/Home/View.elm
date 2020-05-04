module Home.View exposing (view)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile, responsiveVal)
import Helpers.Tuple as TupleHelpers
import Home.Types exposing (..)
import Post exposing (Post)
import Routing exposing (Route)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view : EH.DisplayProfile -> Model -> WalletUXPhaceInfo -> PublishedPostsDict -> Element Msg
view dProfile model walletUXPhaceInfo posts =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color darkTheme.appBackground
        , Element.paddingXY 40 40
            |> changeForMobile (Element.paddingXY 10 20) dProfile
        , Element.Font.color darkTheme.emphasizedTextColor
        ]
    <|
        Element.column
            [ Element.width (Element.fill |> Element.maximum 1100)
            , Element.centerX
            , Element.spacing (110 |> changeForMobile 30 dProfile)
            ]
        <|
            case dProfile of
                Desktop ->
                    [ boldProclamationEl dProfile
                    , Element.column
                        [ Element.width Element.fill
                        , Element.spacing 150
                        ]
                        [ Element.row
                            [ Element.width Element.fill
                            , Element.spacing 40
                            ]
                            [ Element.el [ Element.width Element.fill ] <| topicsBlock dProfile model posts
                            , Element.el [ Element.width Element.fill ] <| topicsExplainerEl dProfile
                            ]
                        , Element.row
                            [ Element.width Element.fill
                            , Element.spacing 40
                            ]
                            [ infoBlock dProfile
                            , composeActionBlock dProfile walletUXPhaceInfo
                            ]
                        ]
                    ]

                Mobile ->
                    [ boldProclamationEl dProfile
                    , infoBlock dProfile
                    , conversationAlreadyStartedEl dProfile
                    , topicsBlock dProfile model posts
                    , topicsExplainerEl dProfile
                    , composeActionBlock dProfile walletUXPhaceInfo
                    ]


boldProclamationEl : DisplayProfile -> Element Msg
boldProclamationEl dProfile =
    Element.column
        [ Element.centerX
        , Element.Font.bold
        , Element.spacing (responsiveVal dProfile 20 10)
        ]
        [ coloredAppTitle
            [ Element.Font.size (responsiveVal dProfile 80 60)
            , Element.centerX
            ]
        , Element.el
            [ Element.width Element.fill
            , Element.paddingXY
                (responsiveVal dProfile 40 15)
                0
            ]
          <|
            EH.thinHRuler <|
                Element.rgb 1 0 0
        , Element.el
            [ Element.Font.size (responsiveVal dProfile 50 30)
            , Element.centerX
            , Element.Font.color Theme.almostWhite
            ]
          <|
            Element.text "A Bunker for Free Speech"
        ]


infoBlock : DisplayProfile -> Element Msg
infoBlock dProfile =
    Element.column
        [ Element.Border.rounded 15
        , Element.Background.color Theme.darkBlue
        , Element.padding (25 |> changeForMobile 15 dProfile)
        , Element.Font.color <| EH.white
        , Element.Font.size (22 |> changeForMobile 18 dProfile)
        , Element.Font.color darkTheme.mainTextColor
        , Element.centerX
        , Element.spacing 20
        , Element.width Element.fill
        , Element.alignTop
        ]
    <|
        List.map
            (Element.paragraph
                [ Element.width Element.fill
                , Element.Font.center
                ]
            )
            [ [ Element.text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat." ]
            , [ Element.column
                    [ Element.spacing 3 ]
                    [ Element.el [ Element.centerX ] <| emphasizedText "No usernames."
                    , Element.el [ Element.centerX ] <| emphasizedText "No moderators."
                    , Element.el [ Element.centerX ] <| emphasizedText "No censorship."
                    , Element.el [ Element.centerX ] <| emphasizedText "No deplatforming."
                    ]
              ]
            , [ Element.text "All you need is ETH for gas and DAI to burn." ]
            , [ Element.text "All SmokeSignal posts are permanent and impossible to delete, and can be accessed with any browser via an IPFS Gateway ("
              , Element.newTabLink
                    [ Element.Font.color defaultTheme.linkTextColor ]
                    { url = "https://gateway.ipfs.io/ipfs/QmeXhVyRJYhtpRcQr4uYsJZi6wBYqyEwdjPRjp3EFCtLHQ/#/context/re?block=9956062&hash=0x0a7e09be33cd207ad208f057e26fba8f8343cfd6c536904c20dbbdf87aa2b257"
                    , label = Element.text "example"
                    }
              , Element.text ") or a SmokeSignal mirror ("
              , Element.newTabLink
                    [ Element.Font.color defaultTheme.linkTextColor ]
                    { url = "https://foundrydao.com/smokesignal/app/#/context/re?block=9956062&hash=0x0a7e09be33cd207ad208f057e26fba8f8343cfd6c536904c20dbbdf87aa2b257"
                    , label = Element.text "example"
                    }
              , Element.text ")."
              ]
            , [ Element.text "Some browsers also support smokesignal.eth links ("
              , Element.newTabLink
                    [ Element.Font.color defaultTheme.linkTextColor ]
                    { url = "https://smokesignal.eth/#/context/re?block=9956062&hash=0x0a7e09be33cd207ad208f057e26fba8f8343cfd6c536904c20dbbdf87aa2b257"
                    , label = Element.text "example"
                    }
              , Element.text ") or direct IPFS links ("
              , Element.newTabLink
                    [ Element.Font.color defaultTheme.linkTextColor ]
                    { url = "ipfs://QmeXhVyRJYhtpRcQr4uYsJZi6wBYqyEwdjPRjp3EFCtLHQ/#/context/re?block=9956062&hash=0x0a7e09be33cd207ad208f057e26fba8f8343cfd6c536904c20dbbdf87aa2b257"
                    , label = Element.text "example"
                    }
              , Element.text ")."
              ]
            ]


conversationAlreadyStartedEl : DisplayProfile -> Element Msg
conversationAlreadyStartedEl dProfile =
    Element.paragraph
        [ Element.Font.size (50 |> changeForMobile 36 dProfile)
        , Element.Font.center
        ]
        [ Element.text "The conversation has already started." ]


topicsExplainerEl : DisplayProfile -> Element Msg
topicsExplainerEl dProfile =
    Element.column
        [ Element.Border.rounded 15
        , Element.Background.color <| Element.rgb 0.3 0 0
        , Element.padding (25 |> changeForMobile 15 dProfile)
        , Element.Font.color <| EH.white
        , Element.Font.size (22 |> changeForMobile 18 dProfile)
        , Element.Font.color darkTheme.mainTextColor
        , Element.centerX
        , Element.width Element.fill
        , Element.spacing 20
        ]
    <|
        List.map
            (Element.paragraph
                [ Element.width Element.fill
                , Element.Font.center
                ]
            )
            [ [ Element.text "Users burn DAI to post messages under any given "
              , emphasizedText "topic"
              , Element.text <|
                    ". Theses topics are listed "
                        ++ responsiveVal dProfile "here" "above"
                        ++ ", along with the "
              , emphasizedText "total DAI burned"
              , Element.text " in that topic."
              ]
            , [ Element.text "If you have a web3 wallet, ETH, and DAI, starting a new topic is easy: type it into the search input, and click "
              , emphasizedText "Start new topic."
              ]
            , [ Element.text " You can then compose the first post for your brand new topic!"
              ]
            ]


composeActionBlock : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
composeActionBlock dProfile walletUXPhaceInfo =
    let
        paragrapher paras =
            Element.column
                [ Element.spacing 15 ]
                (List.map
                    (Element.paragraph
                        [ Element.Font.size (22 |> changeForMobile 18 dProfile)
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
        , Element.width <| Element.px 500
        ]
        [ Element.row
            [ Element.spacing 40
            , Element.centerX
            ]
            [ homeWalletUX dProfile walletUXPhaceInfo
            , Element.column
                [ Element.spacing 5
                , Element.Font.size (40 |> changeForMobile 30 dProfile)
                , Element.Font.bold
                , Element.alignBottom
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
                      , emphasizedText "immutable havoc."
                      , Element.text " Browse the topics above or create your own, or click below to read more about what SmokeSignal can be used for."
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
                    , moreInfoButton dProfile
                    ]

            _ ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ moreInfoButton dProfile
                    , defaultTheme.emphasizedActionButton
                        dProfile
                        [ Element.width Element.fill ]
                        [ "Compose Post" ]
                        (MsgUp <|
                            GotoRoute <|
                                Routing.Compose <|
                                    Post.ForTopic "noob-ramblings-plz-ignore"
                        )
                    ]
        ]


moreInfoButton : DisplayProfile -> Element Msg
moreInfoButton dProfile =
    defaultTheme.secondaryActionButton
        dProfile
        [ Element.width Element.fill ]
        [ "What Can SmokeSignal be Used For?" ]
        (MsgUp <|
            GotoRoute <|
                Routing.ViewContext <|
                    Post.ForPost <|
                        Config.moreInfoPostId
        )


homeWalletUX : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
homeWalletUX dProfile walletUXPhaceInfo =
    Element.map MsgUp <|
        case walletUXPhaceInfo of
            DemoPhaceInfo demoAddress ->
                Element.el
                    [ Element.pointer
                    , Element.Events.onClick <| ConnectToWeb3
                    , Element.Border.rounded 10
                    , Element.Border.glow
                        (Element.rgba 1 0 1 0.3)
                        9
                    ]
                <|
                    phaceElement
                        True
                        MorphingPhace
                        (Eth.Utils.unsafeToAddress demoAddress)
                        False

            UserPhaceInfo ( accountInfo, showAddress ) ->
                Element.el
                    [ Element.Border.rounded 10
                    , Element.Border.glow
                        (Element.rgba 0 0.5 1 0.4)
                        9
                    ]
                <|
                    phaceElement
                        True
                        UserPhace
                        accountInfo.address
                        showAddress


topicsBlock : EH.DisplayProfile -> Model -> PublishedPostsDict -> Element Msg
topicsBlock dProfile model posts =
    Element.column
        [ Element.spacing 25
        , Element.centerX
        , Element.width (Element.fill |> Element.minimum 400)
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
            , topicsColumn
                dProfile
                (Post.sanitizeTopic model.topicInput)
                posts
            ]
        ]


topicsColumn : EH.DisplayProfile -> String -> PublishedPostsDict -> Element Msg
topicsColumn dProfile topicSearchStr allPosts =
    let
        talliedTopics : List ( String, ( (TokenValue, TokenValue), Int ) )
        talliedTopics =
            let
                findTopic : Post.Published -> Maybe String
                findTopic publishedPost =
                    case publishedPost.core.metadata.context of
                        Post.ForTopic topic ->
                            Just topic

                        Post.ForPost postId ->
                            getPublishedPostFromId allPosts postId
                                |> Maybe.andThen findTopic
            in
            allPosts
                |> Dict.values
                |> List.concat
                |> Dict.Extra.filterGroupBy findTopic
                -- This ignores any replies that lead eventually to a postId not in 'posts'
                |> Dict.map
                    (\topic posts ->
                        ( List.foldl
                            (\thisPost ( accBurn, accTip ) ->
                                ( thisPost.core.authorBurn
                                    |> TokenValue.add
                                        (Maybe.withDefault TokenValue.zero thisPost.crowdBurn)
                                    |> TokenValue.add accBurn
                                , Maybe.withDefault TokenValue.zero thisPost.crowdTip
                                    |> TokenValue.add accTip
                                )
                             -- .post
                             --     >> .burnAmount
                             --     >> TokenValue.add
                            )
                            ( TokenValue.zero, TokenValue.zero )
                            posts
                        , List.length posts
                        )
                    )
                |> Dict.toList
                |> List.sortBy (Tuple.second >> Tuple.first >> Tuple.first >> TokenValue.toFloatWithWarning >> negate)

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
                    (\( topic, ( (totalBurned, totalTipped), count ) ) ->
                        Element.row
                            (commonElStyles
                                ++ [ Element.Background.color <| Element.rgba 0 0 1 0.2
                                   , Element.Events.onClick <|
                                        GotoRoute <|
                                            Routing.ViewContext <|
                                                Post.ForTopic topic
                                   ]
                            )
                            [ Element.el
                                [ Element.width <| Element.px 100 ]
                              <|
                                Element.row
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
                                    , Element.text <|
                                        (TokenValue.toConciseString totalBurned
                                            |> (if TokenValue.compare totalBurned (TokenValue.fromIntTokenValue 1) == LT then
                                                    String.left 5

                                                else
                                                    identity
                                               )
                                        )
                                    ]
                            , Element.el
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                , Element.clip
                                ]
                              <|
                                Element.el [ Element.centerY ] <|
                                    Element.text topic
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
                               , Element.clipX
                               , Element.Events.onClick <|
                                    GotoRoute <|
                                        Routing.Compose <|
                                            Post.ForTopic topicSearchStr
                               ]
                        )
                    <|
                        Element.row
                            [ Element.centerY
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
            , Element.width (Element.fill |> Element.maximum 530)
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
