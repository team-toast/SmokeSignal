module View.Home exposing (banner, view)

import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, fillPortion, height, padding, paddingXY, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, white)
import Helpers.Time as TimeHelpers
import Maybe.Extra exposing (unwrap)
import Misc exposing (getPublishedPostFromId)
import Theme exposing (almostWhite, theme)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (Context(..), Id, Model, Msg(..), PhaceIconId(..), Post(..), PostState, Published, PublishedPostsDict, Route(..))
import View.Attrs exposing (cappedWidth, hover, slightRound, whiteGlowAttribute, whiteGlowAttributeSmall)
import View.Common exposing (daiSymbol, phaceElement)
import View.Post
import Wallet


view : Model -> Element Msg
view model =
    let
        dProfile =
            model.dProfile

        donateChecked =
            model.donateChecked

        blockTimes =
            model.blockTimes

        now =
            model.now

        showAddressId =
            model.showAddressId

        demoPhaceSrc =
            model.demoPhaceSrc

        wallet =
            model.wallet

        posts =
            model.publishedPosts

        state =
            { showAddress = False
            , showInput = Types.None
            }
    in
    case dProfile of
        Desktop ->
            [ banner dProfile
            , body dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts state model.searchInput
            ]
                |> column
                    ([ width fill
                     , height fill
                     , spacing 10
                     ]
                        ++ (List.map Element.inFront <|
                                viewModals
                                    dProfile
                                    --model.showNewToSmokeSignalModal
                                    False
                           )
                    )

        Mobile ->
            text "mobile view"


viewModals : DisplayProfile -> Bool -> List (Element Msg)
viewModals dProfile showNewToSmokeSignalModal =
    Maybe.Extra.values
        [ if showNewToSmokeSignalModal == True then
            Just <|
                EH.modal
                    (Element.rgba 0 0 0 0.25)
                    False
                    --CloseNewToSmokeSignalModal
                    --CloseNewToSmokeSignalModal
                    ClickHappened
                    ClickHappened
                <|
                    viewNewToSmokeSignalModal dProfile

          else
            Nothing
        ]


viewNewToSmokeSignalModal : DisplayProfile -> Element Msg
viewNewToSmokeSignalModal dProfile =
    column
        [ whiteGlowAttribute
        , Border.rounded 10
        , Font.color EH.white
        , width fill
        , Background.color <| Element.rgba 0 0 0 0.85
        , padding 50
        , Element.spacing 30
        ]
        [ Element.text "Welcome to" |> rowElement dProfile []
        , rowElement
            dProfile
            []
          <|
            Element.image
                [ width <| Element.px 200
                ]
                { src = "img/smokesignal-logo-vertical.svg"
                , description =
                    "smokesignal logo"
                }
        , rowElement
            dProfile
            []
          <|
            Element.text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat."
        , rowElement
            dProfile
            []
          <|
            el
                [ Font.color Theme.orange, Font.size 28, Font.bold ]
            <|
                Element.text "No Usernames. No Moderators. No censorship. No Deplatforming."
        , rowElement
            dProfile
            []
          <|
            Element.text "All you need is ETH for gas and DAI to burn."
        , rowElement
            dProfile
            []
          <|
            column
                [ Element.spacing 5 ]
                [ rowElement dProfile [] <| Element.text "All SmokeSignal posts are permanent and impossible to delete, and can be"
                , rowElement dProfile [] <| Element.text "accessed with any browser via an IPFS Gateway (example)"
                , rowElement dProfile [] <| Element.text "or the smokesignal.eth.link mirror (example)."
                ]
        , rowElement
            dProfile
            []
          <|
            column
                [ Element.spacing 5 ]
                [ rowElement dProfile [] <| Element.text "If the above two methods prove unreliable, some browsers also support direct"
                , rowElement dProfile [] <| Element.text "smokesignal.eth links (example) or direct IPFS links (example)."
                ]
        , rowElement
            dProfile
            []
          <|
            el
                [ Font.color Theme.orange
                , Font.semiBold
                ]
            <|
                Element.text
                    "Go to introductory video →"
        ]


rowElement : DisplayProfile -> List (Attribute Msg) -> Element Msg -> Element Msg
rowElement dProfile attributes element =
    row
        ([ Element.height fill
         , Element.centerX
         ]
            ++ attributes
        )
        [ element ]


banner : DisplayProfile -> Element Msg
banner dProfile =
    [ text "REAL FREE SPEECH."
    , text "ETERNALLY UNMUTABLE."
    ]
        |> column
            [ spacing 10
            , centerX
            , centerY
            , Font.color EH.white
            , View.Attrs.sanSerifFont
            , Font.bold
            , Font.size 30
            ]
        |> el
            [ width fill
            , Element.height <| Element.px 120
            , Background.color EH.black
            , whiteGlowAttribute
            ]


body :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe PhaceIconId
    -> String
    -> Types.Wallet
    -> PublishedPostsDict
    -> PostState
    -> String
    -> Element Msg
body dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts state searchInput =
    let
        xs =
            Dict.values posts
                |> List.concat
                |> List.take 5

        maybeShowAddressForPostId =
            case showAddressId of
                Just (PhaceForPublishedPost id) ->
                    Just id

                _ ->
                    Nothing
    in
    [ [ "NEW TO SMOKE SIGNAL?"
            |> text
            |> el
                [ View.Attrs.sanSerifFont
                , padding 20
                , slightRound
                , Background.color Theme.orange
                , Font.bold
                , Font.color white
                , Font.size 30
                , whiteGlowAttributeSmall
                , width fill
                ]
      , [ [ [ "Topics"
                |> text
                |> el [ Font.size 35 ]
            , Input.text
                [ width fill
                , Background.color EH.black
                , Border.color Theme.almostWhite
                , whiteGlowAttributeSmall
                , Font.color EH.white
                ]
                { onChange = always ClickHappened
                , text = ""
                , placeholder =
                    Just <|
                        Input.placeholder
                            [ Font.color EH.white
                            , Font.italic
                            ]
                            (Element.text "Find or Create Topic...")
                , label = Input.labelHidden "topic"
                }
            ]
                |> row
                    [ width fill
                    , Background.color black
                    , spacing 50
                    , Font.color white
                    , padding 15
                    ]
          , "MORE RECENT POSTS..."
                |> text
                |> el
                    [ View.Attrs.sanSerifFont
                    , padding 10
                    , slightRound
                    , Background.color Theme.orange
                    , Font.bold
                    , Font.color white
                    , Font.size 20
                    , width fill
                    ]
          ]
            |> column
                [ width fill
                , whiteGlowAttributeSmall
                ]
        , postFeed dProfile donateChecked blockTimes now maybeShowAddressForPostId wallet xs state
        ]
            |> column
                [ width fill
                , Element.spacing 3
                , height fill
                ]
      ]
        |> column
            [ spacing 10
            , width <| fillPortion 2
            , height fill
            ]
    , [ walletUXPane dProfile showAddressId demoPhaceSrc wallet
      , topicsUX dProfile searchInput posts
      ]
        |> column
            [ cappedWidth 400
            , spacing 10
            , height fill
            ]
    ]
        |> row
            [ width fill
            , height fill
            , spacing 10
            ]


orangeBannerEl :
    DisplayProfile
    -> List (Attribute Msg)
    -> Int
    -> Int
    -> String
    -> Element Msg
orangeBannerEl dProfile attributes fontSize paddingVal bannerText =
    el
        ([ width fill
         , padding paddingVal
         , Font.size fontSize
         , Background.color Theme.orange
         , Font.semiBold
         , Font.color EH.white
         , whiteGlowAttribute
         , Border.rounded 10
         ]
            ++ attributes
        )
    <|
        Element.text bannerText


topicsUX : DisplayProfile -> String -> PublishedPostsDict -> Element Msg
topicsUX dProfile topicsSearchInput posts =
    [ Input.button
        [ padding 5
        , slightRound
        , Background.color Theme.orange
        , Font.size 20
        , width fill
        , whiteGlowAttributeSmall
        , hover
        ]
        { onPress = Nothing
        , label =
            text "See All Topics"
                |> el [ centerX ]
        }
    , viewBookmarkedTopics
    , viewTopTrending
    , viewTopVoices
    ]
        |> column
            [ spacing 10
            , width fill
            , height fill
            ]


viewTopTrending : Element msg
viewTopTrending =
    [ text "Top 3 Trending"
        |> el [ centerX ]
        |> el
            [ Font.size 20
            , width fill
            , Background.color Theme.orange
            , slightRound
            , padding 5
            ]
    , [ "Misc"
      , "Sovereign-Network"
      , "Censorship"
      ]
        |> List.map
            (\txt ->
                [ txt
                    |> text
                    |> el [ width fill, Font.size 20 ]
                , 7
                    |> String.fromInt
                    |> text
                    |> el [ Font.size 30, Font.bold ]
                ]
                    |> row
                        [ width fill
                        , whiteGlowAttributeSmall
                        , Background.color black
                        , Font.color white
                        , paddingXY 15 5
                        ]
            )
        |> column [ width fill, height <| px 120, Element.scrollbarY ]
    ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            ]


viewTopVoices : Element Msg
viewTopVoices =
    [ text "Top 3 Voices"
        |> el [ centerX ]
        |> el
            [ Font.size 20
            , width fill
            , Background.color Theme.orange
            , slightRound
            , padding 5
            ]
    , List.range 0 2
        |> List.map
            ([ phaceElement
                ( 50, 50 )
                False
                (Eth.Utils.unsafeToAddress "5257af4ab3b9d719897195658da427dcbbebf048")
                False
                (ShowOrHideAddress DemoPhace)
             , [ "0x10c4...f736"
                    |> text
                    |> el [ Font.size 17 ]
               , "(.eth permalink)"
                    |> text
                    |> el [ Font.size 13 ]
               ]
                |> row [ width fill, spaceEvenly, paddingXY 10 0 ]
             ]
                |> row
                    [ width fill
                    , spaceEvenly
                    , whiteGlowAttributeSmall
                    , Background.color black
                    , Font.color white
                    ]
                |> always
            )
        |> column [ width fill ]
    ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            ]


viewBookmarkedTopics : Element msg
viewBookmarkedTopics =
    [ [ Element.image
            [ height <| px 30
            , width <| px 30
            ]
            { src = "/img/bookmark.svg"
            , description = ""
            }
      , Input.button
            [ Font.size 20
            , width fill
            ]
            { onPress = Nothing
            , label =
                text "Bookmarked Topics"
                    |> el [ centerX ]
            }
      ]
        |> row
            [ width fill
            , height <| px 30
            , Background.color Theme.orange
            , slightRound
            ]
    , [ "Games"
      , "Misc"
      , "Sovereign-Network"
      , "Meta"
      , "Censorship"
      , "SmokeSignal/use-cases"
      ]
        |> List.map
            (\txt ->
                [ txt
                    |> text
                    |> el [ width fill, Font.size 20 ]
                , 7
                    |> String.fromInt
                    |> text
                    |> el [ Font.size 30, Font.bold ]
                ]
                    |> row
                        [ width fill
                        , whiteGlowAttributeSmall
                        , Background.color black
                        , Font.color white
                        , paddingXY 15 5
                        ]
            )
        |> column [ width fill, height <| px 120, Element.scrollbarY ]
    ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            ]


topicsColumn : EH.DisplayProfile -> String -> PublishedPostsDict -> Element Msg
topicsColumn dProfile topicSearchStr allPosts =
    let
        talliedTopics : List ( String, ( ( TokenValue, TokenValue ), Int ) )
        talliedTopics =
            let
                findTopic : Published -> Maybe String
                findTopic publishedPost =
                    case publishedPost.core.metadata.context of
                        TopLevel topic ->
                            Just topic

                        Reply postId ->
                            getPublishedPostFromId allPosts postId
                                |> Maybe.andThen findTopic
            in
            allPosts
                |> Dict.values
                |> List.concat
                |> Dict.Extra.filterGroupBy findTopic
                -- This ignores any replies that lead eventually to a postId not in 'posts'
                |> Dict.map
                    (\_ posts ->
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
            , padding 5
            , Border.width 1
            , Border.color Theme.almostWhite
            , width fill
            , Element.pointer
            , Element.height <| Element.px 40
            , whiteGlowAttributeSmall
            , Font.color EH.white
            , Background.color EH.black
            ]

        exactTopicFound =
            talliedTopics
                |> List.any (Tuple.first >> (==) topicSearchStr)

        maybeCreateTopicEl =
            if topicSearchStr /= "" && not exactTopicFound then
                Just <|
                    el
                        (commonElStyles
                            ++ [ Element.Events.onClick <|
                                    GotoRoute <|
                                        Compose <|
                                            TopLevel topicSearchStr
                               ]
                        )
                    <|
                        row
                            [ Element.centerY
                            ]
                            [ Element.text "Start new topic "
                            , el
                                [ Font.italic
                                , Font.bold
                                ]
                              <|
                                Element.text topicSearchStr
                            ]

            else
                Nothing
    in
    filteredTalliedTopics
        |> List.map
            (\( topic, ( ( totalBurned, totalTipped ), count ) ) ->
                Input.button commonElStyles
                    { onPress =
                        RouteTopic topic
                            |> GotoRoute
                            |> Just
                    , label =
                        [ [ daiSymbol theme.daiBurnedTextIsWhite [ Element.height <| Element.px 18 ]
                          , TokenValue.toConciseString totalBurned
                                |> (if TokenValue.compare totalBurned (TokenValue.fromIntTokenValue 1) == LT then
                                        String.left 5

                                    else
                                        identity
                                   )
                                |> text
                          ]
                            |> row
                                [ Element.spacing 3
                                , padding 5
                                , Border.rounded 5
                                , Background.color theme.daiBurnedBackground
                                , Font.color
                                    (if theme.daiBurnedTextIsWhite then
                                        EH.white

                                     else
                                        EH.black
                                    )
                                ]
                            |> el
                                [ width <| px 100
                                ]
                        , [ text topic
                                |> el
                                    [ Font.color EH.white
                                    ]
                          , el
                                [ Element.alignRight
                                , Font.color EH.white
                                ]
                            <|
                                Element.text <|
                                    String.fromInt count
                          ]
                            |> row [ spaceEvenly, width fill ]
                        ]
                            |> row [ width fill ]
                    }
            )
        |> (++)
            (maybeCreateTopicEl
                |> unwrap [] List.singleton
            )
        |> column
            [ Border.roundEach
                { topRight = 0
                , topLeft = 0
                , bottomRight = 5
                , bottomLeft = 5
                }
            , width (fill |> Element.maximum 530)
            , padding 5
            , Element.spacing 5
            , Background.color EH.black
            , Element.scrollbarY
            , height fill
            ]


walletUXPane :
    DisplayProfile
    -> Maybe PhaceIconId
    -> String
    -> Types.Wallet
    -> Element Msg
walletUXPane dProfile showAddressId demoPhaceSrc wallet =
    let
        phaceEl =
            case Wallet.userInfo wallet of
                Nothing ->
                    phaceElement
                        ( 80, 80 )
                        True
                        (Eth.Utils.unsafeToAddress demoPhaceSrc)
                        (showAddressId == Just DemoPhace)
                        (ShowOrHideAddress DemoPhace)
                        |> el
                            [ Border.rounded 10
                            , Border.glow
                                (Element.rgba 1 0 1 0.3)
                                9
                            ]

                Just userInfo ->
                    phaceElement
                        ( 100, 100 )
                        True
                        userInfo.address
                        (showAddressId == Just UserPhace)
                        (ShowOrHideAddress UserPhace)
                        |> el
                            [ Border.rounded 10
                            , Border.glow
                                (Element.rgba 0 0.5 1 0.4)
                                9
                            ]

        ( buttonText, maybeButtonAction, maybeExplainerText ) =
            case wallet of
                Types.NoneDetected ->
                    ( "Install Metamask"
                    , Just <| EH.NewTabLink "https://metamask.io/"
                    , Just "Then come back to try on some phaces!"
                    )

                Types.OnlyNetwork _ ->
                    ( "Connect Wallet"
                    , Just <| EH.Action ConnectToWeb3
                    , Just "Each address has a unique phace!"
                    )

                Types.Active userInfo ->
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
                        , Just <| EH.Action <| GotoRoute <| Compose <| TopLevel Misc.defaultTopic
                        , Nothing
                        )

        button =
            let
                attributes =
                    [ paddingXY 10 5
                    , width fill
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
                            [ Font.color EH.white
                            , Font.size 16
                            ]
                            [ Element.text text ]
                    )
                |> Maybe.withDefault Element.none
    in
    [ phaceEl
    , column
        [ width fill
        , spaceEvenly
        , height fill
        ]
        [ button
        , explainerParagraphOrNone
        ]
    ]
        |> row
            [ width fill
            , spacing 10
            ]


postFeed :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Id
    -> Types.Wallet
    -> List Published
    -> PostState
    -> Element Msg
postFeed dProfile donateChecked blockTimes now maybeShowAddressForId wallet listOfPosts state =
    listOfPosts
        |> List.sortBy (feedSortByFunc blockTimes now)
        |> List.reverse
        |> List.map
            (\post ->
                View.Post.view
                    dProfile
                    donateChecked
                    (maybeShowAddressForId == Just post.id)
                    blockTimes
                    now
                    wallet
                    state
                    post
            )
        |> column
            [ width fill
            , height fill
            , Element.scrollbarY
            , spacing 5
            , paddingXY 0 5
            ]


feedSortByFunc : Dict Int Time.Posix -> Time.Posix -> (Published -> Float)
feedSortByFunc blockTimes now =
    \post ->
        let
            postTimeDefaultZero =
                blockTimes
                    |> Dict.get post.id.block
                    |> Maybe.withDefault (Time.millisToPosix 0)

            age =
                TimeHelpers.sub now postTimeDefaultZero

            --ageFactor =
            _ =
                -- 1 at age zero, falls to 0 when 3 days old
                TimeHelpers.getRatio
                    age
                    (TimeHelpers.mul TimeHelpers.oneDay 90)
                    |> clamp 0 1
                    |> (\ascNum -> 1 - ascNum)

            totalBurned =
                Misc.totalBurned (PublishedPost post)
                    |> TokenValue.toFloatWithWarning

            newnessMultiplier =
                1

            -- (ageFactor * 4.0) + 1
        in
        totalBurned * newnessMultiplier
