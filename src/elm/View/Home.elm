module View.Home exposing (view)

import Dict exposing (Dict)
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, padding, paddingXY, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, white)
import Helpers.Time as TimeHelpers
import Set
import Theme exposing (orange)
import Time
import TokenValue
import Types exposing (..)
import View.Attrs exposing (hover, slightRound, whiteGlowAttribute, whiteGlowAttributeSmall)
import View.Common exposing (phaceElement)
import View.Img
import View.Post


view : Model -> Element Msg
view model =
    case model.dProfile of
        Desktop ->
            viewDesktop model

        Mobile ->
            text "mobile view"


viewDesktop : Model -> Element Msg
viewDesktop model =
    let
        posts =
            Dict.values model.rootPosts
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
    , posts
        |> List.sortBy (feedSortByFunc model.blockTimes model.now)
        |> List.reverse
        |> List.map (viewPost model)
        |> column
            [ width fill
            , height fill
            , spacing 5
            , paddingXY 0 5
            ]
    ]
        |> column
            [ spacing 10
            , width fill
            , height fill
            ]


viewPost : Model -> RootPost -> Element Msg
viewPost model post =
    View.Post.view
        model.dProfile
        (model.blockTimes
            |> Dict.get post.core.id.block
        )
        model.now
        (model.replyIds
            |> Dict.get post.core.key
            |> Maybe.withDefault Set.empty
        )
        (model.accounting
            |> Dict.get post.core.key
        )
        (model.tipOpen
            |> Maybe.andThen
                (\x ->
                    if x.id == post.core.id then
                        Just x.showInput

                    else
                        Nothing
                )
        )
        model.ethPrice
        model.compose.dai
        post.topic
        post.core


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


feedSortByFunc : Dict Int Time.Posix -> Time.Posix -> RootPost -> Float
feedSortByFunc blockTimes now post =
    let
        postTimeDefaultZero =
            blockTimes
                |> Dict.get post.core.id.block
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
            --Misc.totalBurned (PublishedPost post)
            post.core.authorBurn
                |> TokenValue.toFloatWithWarning

        newnessMultiplier =
            (ageFactor * 4.0) + 1
    in
    totalBurned * newnessMultiplier
