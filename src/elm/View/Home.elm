module View.Home exposing (banner, viewOverview, viewTopic)

import Dict exposing (Dict)
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, padding, paddingXY, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, white)
import Helpers.Time as TimeHelpers
import Maybe.Extra
import Misc
import Set exposing (Set)
import Theme exposing (almostWhite, orange)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (cappedWidth, hover, slightRound, whiteGlowAttribute, whiteGlowAttributeSmall)
import View.Common exposing (phaceElement)
import View.Img
import View.Post
import View.Topic
import Wallet


view : Model -> Element Msg
view model =
    case model.dProfile of
        Desktop ->
            --viewDesktop model
            text "mobile view"

        Mobile ->
            text "mobile view"


viewModals : DisplayProfile -> Bool -> List (Element Msg)
viewModals dProfile showNewToSmokeSignalModal =
    Maybe.Extra.values
        [ if showNewToSmokeSignalModal == True then
            --Just <|
            --EH.modal
            --(Element.rgba 0 0 0 0.25)
            --False
            --CloseNewToSmokeSignalModal
            --CloseNewToSmokeSignalModal
            --<|
            --viewNewToSmokeSignalModal dProfile
            Nothing

          else
            Nothing
        ]


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
            , View.Attrs.sansSerifFont
            , Font.bold
            , Font.size 30
            ]
        |> el
            [ width fill
            , Element.height <| Element.px 120
            , Background.color EH.black
            , whiteGlowAttribute
            ]


viewFrame : Model -> Element Msg -> Element Msg
viewFrame model elem =
    [ banner model.dProfile
    , [ elem
      , [ walletUXPane model.dProfile model.showAddressId model.demoPhaceSrc model.wallet
        , viewTopics model.topics
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
    ]
        |> column
            ([ width fill
             , height fill
             , spacing 10
             ]
                ++ (List.map Element.inFront <|
                        viewModals
                            model.dProfile
                            --model.showNewToSmokeSignalModal
                            False
                   )
            )


viewOverview : Model -> Element Msg
viewOverview model =
    let
        posts =
            Dict.values model.rootPosts

        maybeShowAddressForPostId =
            case showAddressId of
                Just (PhaceForPublishedPost id) ->
                    Just id

                _ ->
                    Nothing

        dProfile =
            model.dProfile

        blockTimes =
            model.blockTimes

        now =
            model.now

        showAddressId =
            model.showAddressId

        demoPhaceSrc =
            model.demoPhaceSrc

        state =
            { showAddress = False
            , showInput = Types.None
            }
    in
    [ Input.button
        [ View.Attrs.sansSerifFont
        , padding 20
        , slightRound
        , Background.color Theme.orange
        , Font.bold
        , Font.color white
        , Font.size 30
        , whiteGlowAttributeSmall
        , width fill
        , hover
        ]
        { onPress = Just <| ShowNewToSmokeSignalModal True
        , label =
            "NEW TO SMOKE SIGNAL?"
                |> text
                |> Element.el [ centerX ]
        }
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
                [ View.Attrs.sansSerifFont
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
      , posts
            --|> List.sortBy (feedSortByFunc blockTimes now)
            |> List.reverse
            |> List.map
                (\post ->
                    View.Post.view
                        dProfile
                        (model.blockTimes
                            |> Dict.get post.core.id.block
                        )
                        now
                        (model.replyIds
                            |> Dict.get post.core.key
                            |> Maybe.withDefault Set.empty
                        )
                        (model.accounting
                            |> Dict.get post.core.key
                        )
                        post.topic
                        post.core
                )
            |> column
                [ width fill
                , height fill
                , spacing 5
                , paddingXY 0 5
                ]
      ]
        |> column
            [ width fill
            , Element.spacing 3
            , height fill
            ]
    ]
        |> column
            [ spacing 10
            , width fill
            , height fill
            ]
        |> viewFrame model


viewTopic : Model -> String -> Element Msg
viewTopic model topic =
    View.Topic.view model topic
        |> viewFrame model


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


topicsUX : DisplayProfile -> String -> Element Msg
topicsUX dProfile topicsSearchInput =
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
        |> column
            [ width fill
            , height <| px 120
            , Element.scrollbarY
            ]
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


viewBookmarkedTopics : Element Msg
viewBookmarkedTopics =
    [ [ View.Img.bookmark 17 orange
            |> el [ centerX, centerY ]
            |> el [ height <| px 30, width <| px 30, Background.color black ]
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
            (\topic ->
                Input.button
                    [ width fill
                    , whiteGlowAttributeSmall
                    , Background.color black
                    , Font.color white
                    , paddingXY 15 5
                    , hover
                    ]
                    { onPress = Just <| GotoView <| ViewTopic topic
                    , label =
                        [ topic
                            |> text
                            |> el [ width fill, Font.size 20 ]
                        , 7
                            |> String.fromInt
                            |> text
                            |> el [ Font.size 30, Font.bold ]
                        ]
                            |> row
                                [ width fill
                                ]
                    }
            )
        |> column [ width fill, height <| px 120, Element.scrollbarY ]
    ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            ]


viewTopics : Dict String TokenValue -> Element Msg
viewTopics topics =
    [ [ View.Img.bookmark 17 orange
            |> el [ centerX, centerY ]
            |> el [ height <| px 30, width <| px 30, Background.color black ]
      , text "Topics"
            |> el [ centerX ]
      ]
        |> row
            [ width fill
            , height <| px 30
            , Background.color Theme.orange
            , slightRound
            ]
    , topics
        |> Dict.toList
        |> List.sortBy
            (Tuple.second
                >> TokenValue.toFloatWithWarning
                >> negate
            )
        |> List.map
            (\( topic, totalBurned ) ->
                Input.button
                    [ width fill
                    , whiteGlowAttributeSmall
                    , Background.color black
                    , Font.color white
                    , paddingXY 15 5
                    , hover
                    ]
                    { onPress = Just <| GotoView <| ViewTopic topic
                    , label =
                        [ topic
                            |> text
                            |> el [ width fill, Font.size 20 ]
                        , totalBurned
                            |> TokenValue.toConciseString
                            |> text
                            |> el [ Font.size 30, Font.bold ]
                        ]
                            |> row
                                [ width fill
                                ]
                    }
            )
        |> column [ width fill ]
    ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
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
                        , Just <| EH.Action <| ComposeToggle
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
