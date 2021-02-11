module View.Home exposing (view)

import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, height, padding, paddingXY, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), white)
import Helpers.Time as TimeHelpers
import Set
import Theme exposing (orange)
import Time
import TokenValue
import Types exposing (..)
import View.Attrs exposing (hover, slightRound, whiteGlowAttributeSmall)
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
        model.compose.dollar
        post.topic
        post.core


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
