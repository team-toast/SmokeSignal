module Home.View exposing (view)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Embed.Youtube
import Embed.Youtube.Attributes
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Time as TimeHelpers
import Helpers.Tuple as TupleHelpers
import Home.Types exposing (..)
import Html.Attributes exposing (list)
import Maybe.Extra
import Post exposing (Post)
import PostUX.Preview as PostPreview
import PostUX.Types as PostUX
import Routing exposing (Route)
import Theme exposing (almostWhite, theme)
import Time
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view :
    EH.DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe PhaceIconId
    -> String
    -> Wallet
    -> PublishedPostsDict
    -> Model
    -> Element Msg
view dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model =
    case dProfile of
        Desktop ->
            Element.column
                ([ Element.centerX
                 , Element.width <| Element.px 1000
                 , Element.spacing majorSpacing
                 ]
                    ++ (List.map Element.inFront <|
                            viewModals
                                dProfile
                                model.showNewToSmokeSignalModal
                       )
                )
                [ Element.el
                    [ Element.width Element.fill
                    , Element.paddingXY 10 0
                    ]
                    banner
                , body dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model
                ]

        Mobile ->
            Debug.todo ""


viewModals :
    DisplayProfile
    -> Bool
    -> List (Element Msg)
viewModals dProfile showNewToSmokeSignalModal =
    Maybe.Extra.values
        [ if showNewToSmokeSignalModal == True then
            Just <|
                EH.modal
                    (Element.rgba 0 0 0 0.25)
                    False
                    CloseNewToSmokeSignalModal
                    CloseNewToSmokeSignalModal
                <|
                    viewNewToSmokeSignalModal dProfile

          else
            Nothing
        ]


viewNewToSmokeSignalModal :
    DisplayProfile
    -> Element Msg
viewNewToSmokeSignalModal dProfile =
    Element.column
        [ whiteGlowAttribute
        , Element.Border.rounded 10
        , Element.Font.color EH.white
        , Element.width Element.fill
        , Element.height <| Element.px 600
        , Element.Background.color <| Element.rgba 0 0 0 0.85
        ]
        [ Element.text "Welcome to" |> rowElement dProfile [ Element.centerX ]
        , Element.image
            [ Element.width <| Element.px 150
            , Element.centerX
            ]
            { src = "img/smokesignal-logo-vertical.svg"
            , description =
                "smokesignal logo"
            }
            |> rowElement dProfile []
        , Element.paragraph [ Element.centerX ] [ Element.text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat." ] |> rowElement dProfile []
        , Element.paragraph [] [ Element.text "No Usernames. No Moderators. No censorship. No Deplatforming." ] |> rowElement dProfile []
        , Element.paragraph [] [ Element.text "All you need is ETH for gas and DAI to burn." ] |> rowElement dProfile []
        , Element.paragraph [] [ Element.text "All SmokeSignal posts are permanent and impossible to delete, and can be accessed with any browser via an IPFS Gateway (example) or the smokesignal.eth.link mirror (example)." ] |> rowElement dProfile []
        , Element.paragraph [] [ Element.text "If the above two methods prove unreliable, some browsers also support direct smokesignal.eth links (example) or direct IPFS links (example)." ] |> rowElement dProfile []
        , Element.paragraph [] [ Element.text "Go to introductory video -->" ] |> rowElement dProfile []
        ]


rowElement : DisplayProfile -> List (Attribute Msg) -> Element Msg -> Element Msg
rowElement dProfile attributes element =
    Element.row
        ([ Element.width Element.fill
         , Element.height Element.fill
         , Element.centerX
         ]
            ++ attributes
        )
        [ element ]


banner : Element Msg
banner =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 200
        , Element.Background.color EH.black
        , whiteGlowAttribute
        ]
    <|
        Element.el
            [ Element.centerX
            ]
            (Element.text "hi")


body :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe PhaceIconId
    -> String
    -> Wallet
    -> PublishedPostsDict
    -> Model
    -> Element Msg
body dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model =
    Element.row
        [ Element.width Element.fill
        , Element.spacing majorSpacing
        ]
        [ Element.column
            [ Element.spacing majorSpacing
            , Element.alignTop
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            , Element.clip
            , Element.width <| Element.fillPortion 2
            , Element.padding 10
            ]
            [ orangeBannerEl
                dProfile
                [ Element.pointer
                , Element.Events.onClick ShowNewToSmokeSignalModal
                ]
                40
                20
                "NEW TO SMOKESIGNAL?"
            , Element.column
                [ Element.width Element.fill
                , Element.spacing 3
                ]
                [ orangeBannerEl
                    dProfile
                    []
                    20
                    10
                    "RECENT POSTS..."
                , Element.column
                    [ Element.width Element.fill
                    , Element.clipX
                    ]
                    [ let
                        maybeShowAddressForPostId =
                            case showAddressId of
                                Just (PhaceForPublishedPost id) ->
                                    Just id

                                _ ->
                                    Nothing
                      in
                      mainPostFeed dProfile donateChecked blockTimes now maybeShowAddressForPostId wallet posts
                    ]
                ]
            ]
        , Element.column
            [ Element.width <| Element.fillPortion 1
            , Element.alignTop
            , Element.padding 10
            , Element.spacing 20
            ]
            [ walletUXPane dProfile showAddressId demoPhaceSrc wallet
            , topicsUX dProfile model.topicSearchInput posts
            ]
        ]


orangeBannerEl :
    DisplayProfile
    -> List (Attribute Msg)
    -> Int
    -> Int
    -> String
    -> Element Msg
orangeBannerEl dProfile attributes fontSize padding bannerText =
    Element.el
        ([ Element.width Element.fill
         , Element.padding padding
         , Element.Font.size fontSize
         , Element.Background.color Theme.orange
         , Element.Font.semiBold
         , Element.Font.color EH.white
         , whiteGlowAttribute
         , Element.Border.rounded 10
         ]
            ++ attributes
        )
    <|
        Element.text bannerText


topicsUX :
    DisplayProfile
    -> String
    -> PublishedPostsDict
    -> Element Msg
topicsUX dProfile topicsSearchInput posts =
    Element.column
        [ Element.spacing 10
        , Element.centerX
        , Element.width (Element.fill |> Element.minimum 400)
        ]
        [ orangeBannerEl
            dProfile
            []
            26
            12
            "TOPICS"
        , Element.column
            [ Element.width Element.fill
            , Element.alignTop
            , Element.spacing 1
            ]
            [ Element.Input.text
                [ Element.width Element.fill
                , Element.Background.color EH.black
                , Element.Border.color Theme.almostWhite
                , whiteGlowAttributeSmall
                , Element.Font.color EH.white
                ]
                { onChange = SearchInputChanged
                , text = topicsSearchInput
                , placeholder =
                    Just <|
                        Element.Input.placeholder
                            [ Element.Font.color EH.white
                            , Element.Font.italic
                            ]
                            (Element.text "Find or Create Topic...")
                , label = Element.Input.labelHidden "topic"
                }
            , topicsColumn
                dProfile
                (Post.sanitizeTopic topicsSearchInput)
                posts
            ]
        ]


topicsColumn :
    EH.DisplayProfile
    -> String
    -> PublishedPostsDict
    -> Element Msg
topicsColumn dProfile topicSearchStr allPosts =
    let
        talliedTopics : List ( String, ( ( TokenValue, TokenValue ), Int ) )
        talliedTopics =
            let
                findTopic : Post.Published -> Maybe String
                findTopic publishedPost =
                    case publishedPost.core.metadata.context of
                        Post.TopLevel topic ->
                            Just topic

                        Post.Reply postId ->
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
                                case thisPost.maybeAccounting of
                                    Just accounting ->
                                        ( TokenValue.add
                                            accounting.totalBurned
                                            accBurn
                                        , TokenValue.add
                                            accounting.totalTipped
                                            accTip
                                        )

                                    Nothing ->
                                        ( TokenValue.add
                                            thisPost.core.authorBurn
                                            accBurn
                                        , accTip
                                        )
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
            , Element.Border.color Theme.almostWhite
            , Element.width Element.fill
            , Element.pointer
            , Element.height <| Element.px 40
            , whiteGlowAttributeSmall
            , Element.Font.color EH.white
            , Element.Background.color EH.black
            ]

        topicEls =
            filteredTalliedTopics
                |> List.map
                    (\( topic, ( ( totalBurned, totalTipped ), count ) ) ->
                        Element.row
                            (commonElStyles
                                ++ [ Element.Events.onClick <|
                                        GotoRoute <|
                                            Routing.ViewContext <|
                                                Topic topic
                                   ]
                            )
                            [ Element.el
                                [ Element.width <| Element.px 100 ]
                              <|
                                Element.row
                                    [ Element.padding 5
                                    , Element.spacing 3
                                    , Element.Border.rounded 5
                                    , Element.Background.color theme.daiBurnedBackground
                                    , Element.Font.color
                                        (if theme.daiBurnedTextIsWhite then
                                            EH.white

                                         else
                                            EH.black
                                        )
                                    ]
                                    [ daiSymbol theme.daiBurnedTextIsWhite [ Element.height <| Element.px 18 ]
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
                                Element.el
                                    [ Element.centerY
                                    , Element.Font.color EH.white
                                    ]
                                <|
                                    Element.text topic
                            , Element.el
                                [ Element.alignRight
                                , Element.Font.color EH.white
                                ]
                              <|
                                Element.text <|
                                    String.fromInt count
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
                            ++ [ Element.clipX
                               , Element.Events.onClick <|
                                    GotoRoute <|
                                        Routing.Compose <|
                                            Post.TopLevel topicSearchStr
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
            , Element.padding 5
            , Element.spacing 5
            , Element.Background.color EH.black
            ]
            ((Maybe.map List.singleton maybeCreateTopicEl
                |> Maybe.withDefault []
             )
                ++ topicEls
            )


mainPostFeed :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Post.Id
    -> Wallet
    -> PublishedPostsDict
    -> Element Msg
mainPostFeed dProfile donateChecked blockTimes now maybeShowAddressForPostId wallet posts =
    Element.row
        [ Element.width Element.fill
        , Element.spacing majorSpacing
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clip
            ]
          <|
            [ postFeed
                dProfile
                donateChecked
                blockTimes
                now
                maybeShowAddressForPostId
                wallet
                (Dict.values posts |> List.concat)
            ]
        ]


walletUXPane :
    DisplayProfile
    -> Maybe PhaceIconId
    -> String
    -> Wallet
    -> Element Msg
walletUXPane dProfile showAddressId demoPhaceSrc wallet =
    let
        phaceEl =
            case Wallet.userInfo wallet of
                Nothing ->
                    Element.el
                        [ Element.Border.rounded 10
                        , Element.Border.glow
                            (Element.rgba 1 0 1 0.3)
                            9
                        ]
                    <|
                        phaceElement
                            ( 100, 100 )
                            True
                            (Eth.Utils.unsafeToAddress demoPhaceSrc)
                            (showAddressId == Just DemoPhace)
                            (ShowOrHideAddress DemoPhace)
                            NoOp

                Just userInfo ->
                    Element.el
                        [ Element.Border.rounded 10
                        , Element.Border.glow
                            (Element.rgba 0 0.5 1 0.4)
                            9
                        ]
                    <|
                        phaceElement
                            ( 100, 100 )
                            True
                            userInfo.address
                            (showAddressId == Just UserPhace)
                            (ShowOrHideAddress UserPhace)
                            NoOp

        ( buttonText, maybeButtonAction, maybeExplainerText ) =
            case wallet of
                Wallet.NoneDetected ->
                    ( "Install Metamask"
                    , Just <| EH.NewTabLink "https://metamask.io/"
                    , Just "Then come back to try on some phaces!"
                    )

                Wallet.OnlyNetwork _ ->
                    ( "Connect Wallet"
                    , Just <| EH.Action ConnectToWeb3
                    , Just "Each address has a unique phace!"
                    )

                Wallet.Active userInfo ->
                    let
                        userHasNoEth =
                            userInfo.balance
                                |> Maybe.map TokenValue.isZero
                                |> Maybe.withDefault False
                    in
                    if userHasNoEth then
                        ( "Compose Post"
                        , Nothing
                        , Just "That address has no ETH! You need ETH to post on SmokeSignal."
                        )

                    else
                        ( "Compose Post"
                        , Just <| EH.Action <| GotoRoute <| Routing.Compose <| Post.TopLevel Post.defaultTopic
                        , Nothing
                        )

        button =
            let
                attributes =
                    [ Element.paddingXY 10 5
                    , Element.width Element.fill
                    ]
                        ++ (case maybeExplainerText of
                                Nothing ->
                                    [ Element.centerY
                                    ]

                                _ ->
                                    []
                           )
            in
            case maybeButtonAction of
                Just buttonAction ->
                    Theme.unscaryButton
                        dProfile
                        attributes
                        [ buttonText ]
                        buttonAction

                Nothing ->
                    Theme.disabledButton
                        dProfile
                        attributes
                        buttonText

        explainerParagraphOrNone =
            maybeExplainerText
                |> Maybe.map
                    (\text ->
                        Element.paragraph
                            [ Element.Font.color EH.white
                            , Element.Font.size 16
                            ]
                            [ Element.text text ]
                    )
                |> Maybe.withDefault Element.none
    in
    Element.map MsgUp <|
        Element.row
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ phaceEl
            , Element.column
                [ Element.width Element.fill
                , Element.spacing 15
                , Element.height Element.fill
                ]
                [ button
                , explainerParagraphOrNone
                ]
            ]



-- stuff =
--     case walletUXPhaceInfo of
--         DemoPhaceInfo demoAddress ->
--             Element.el
--                 [ Element.pointer
--                 , Element.Border.rounded 10
--                 , Element.Border.glow
--                     (Element.rgba 1 0 1 0.3)
--                     9
--                 ]
--             <|
--                 phaceElement
--                     ( 100, 100 )
--                     True
--                     (Eth.Utils.unsafeToAddress demoAddress)
--                     False
--                     (ShowOrHideAddress DemoPhace)
--                     NoOp
-- UserPhaceInfo ( accountInfo, showAddress ) ->
--     Element.el
--         [ Element.Border.rounded 10
--         , Element.Border.glow
--             (Element.rgba 0 0.5 1 0.4)
--             9
--         ]
--     <|
--         phaceElement
--             ( 100, 100 )
--             True
--             accountInfo.address
--             showAddress
--             (ShowOrHideAddress UserPhace)
--             NoOp
-- Element.el
--     [ Element.width Element.fill
--     , Element.height Element.fill
--     , responsiveVal dProfile
--         (Element.paddingXY 40 40)
--         (Element.paddingXY 10 20)
--     , Element.Font.color theme.emphasizedTextColor
--     ]
-- <|
--     Element.column
--         [ Element.width (Element.fill |> Element.maximum 1100)
--         , Element.centerX
--         , Element.spacing (responsiveVal dProfile 50 30)
--         ]
--     <|
--         case dProfile of
--             Desktop ->
--                 [ boldProclamationEl dProfile
--                 , Element.row
--                     [ Element.width Element.fill
--                     , Element.spacing 40
--                     , Element.clip
--                     , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
--                     ]
--                     [ Element.column
--                         [ Element.width Element.fill
--                         , Element.height Element.fill
--                         , Element.clip
--                         ]
--                       <|
--                         [ postFeed
--                             dProfile
--                             donateChecked
--                             blockTimes
--                             now
--                             maybeShowAddressForPostId
--                             (Dict.values posts |> List.concat)
--                         ]
--                     ]
--                 ]
--             Mobile ->
--                 Debug.todo ""


majorSpacing : Int
majorSpacing =
    20



-- [ boldProclamationEl dProfile
-- , tutorialVideo dProfile
-- , conversationAlreadyStartedEl dProfile
-- , case walletUXPhaceInfo of
--     DemoPhaceInfo _ ->
--         web3ConnectButton
--             dProfile
--             [ Element.width Element.fill ]
--             MsgUp
--     _ ->
--         Element.column
--             [ Element.width Element.fill
--             , Element.spacing 10
--             ]
--             [ theme.greenActionButton
--                 dProfile
--                 [ Element.width Element.fill ]
--                 [ "Create a New Post" ]
--                 (MsgUp <|
--                     GotoRoute <|
--                         Routing.Compose <|
--                             Post.TopLevel Post.defaultTopic
--                 )
--             ]
--, infoBlock dProfile
--, conversationAlreadyStartedEl dProfile
-- , topicsBlock dProfile posts
-- , topicsExplainerEl dProfile
--, composeActionBlock dProfile walletUXPhaceInfo
-- ]


postFeed :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Post.Id
    -> Wallet
    -> List Post.Published
    -> Element Msg
postFeed dProfile donateChecked blockTimes now maybeShowAddressForId wallet listOfPosts =
    let
        posts =
            List.sortBy (feedSortByFunc blockTimes now)
                listOfPosts
                |> List.reverse
                |> List.take 10
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacingXY 0 5
        , Element.paddingXY 0 5
        ]
    <|
        List.map
            (previewPost
                dProfile
                donateChecked
                blockTimes
                now
                maybeShowAddressForId
                wallet
                Nothing
            )
            posts


feedSortByFunc :
    Dict Int Time.Posix
    -> Time.Posix
    -> (Post.Published -> Float)
feedSortByFunc blockTimes now =
    \post ->
        let
            postTimeDefaultZero =
                blockTimes
                    |> Dict.get post.id.block
                    |> Maybe.withDefault (Time.millisToPosix 0)

            age =
                TimeHelpers.sub now postTimeDefaultZero

            ageFactor =
                -- 1 at age zero, falls to 0 when 3 days old
                TimeHelpers.getRatio
                    age
                    (TimeHelpers.mul TimeHelpers.oneDay 90)
                    |> clamp 0 1
                    |> (\ascNum -> 1 - ascNum)

            totalBurned =
                Post.totalBurned (Post.PublishedPost post)
                    |> TokenValue.toFloatWithWarning

            newnessMultiplier =
                1

            -- (ageFactor * 4.0) + 1
        in
        totalBurned * newnessMultiplier


previewPost :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Post.Id
    -> Wallet
    -> Maybe PostUX.Model
    -> Post.Published
    -> Element Msg
previewPost dProfile donateChecked blockTimes now maybeShowAddressForId wallet maybePostUXModel post =
    Element.map PostUXMsg <|
        PostPreview.view
            dProfile
            donateChecked
            (maybeShowAddressForId == Just post.id)
            blockTimes
            now
            wallet
            maybePostUXModel
            post



-- homeWalletUX : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
-- homeWalletUX dProfile walletUXPhaceInfo =
--     Element.map MsgUp <|
--         case walletUXPhaceInfo of
--             DemoPhaceInfo demoAddress ->
--                 Element.el
--                     [ Element.pointer
--                     , Element.Events.onClick <| ConnectToWeb3
--                     , Element.Border.rounded 10
--                     , Element.Border.glow
--                         (Element.rgba 1 0 1 0.3)
--                         9
--                     ]
--                 <|
--                     phaceElement
--                         ( 100, 100 )
--                         True
--                         (Eth.Utils.unsafeToAddress demoAddress)
--                         False
--                         (ShowOrHideAddress MorphingPhace)
--                         NoOp
--             UserPhaceInfo ( accountInfo, showAddress ) ->
--                 Element.el
--                     [ Element.Border.rounded 10
--                     , Element.Border.glow
--                         (Element.rgba 0 0.5 1 0.4)
--                         9
--                     ]
--                 <|
--                     phaceElement
--                         ( 100, 100 )
--                         True
--                         accountInfo.address
--                         showAddress
--                         (ShowOrHideAddress UserPhace)
--                         NoOp
