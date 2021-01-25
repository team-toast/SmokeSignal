module View.Topic exposing (view)

import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, height, padding, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile, black, white)
import Theme exposing (orange, theme)
import Time
import Types exposing (Context(..), Model, Msg, Published, PublishedPostsDict, Wallet)
import View.Attrs exposing (cappedWidth, hover, slightRound, whiteGlowAttribute, whiteGlowAttributeSmall)
import View.Img
import View.Post


view : Model -> String -> Element Msg
view model topic =
    [ topicHeader topic
    , viewPosts model.dProfile
        model.donateChecked
        False
        topic
        model.blockTimes
        model.now
        model.wallet
        model.publishedPosts
    ]
        |> column
            [ width fill
            , height fill
            , spacing 10
            , Element.alignTop
            ]


topicHeader : String -> Element Msg
topicHeader topic =
    [ [ topic
            |> text
            |> el [ Font.size 35 ]
      , View.Img.bookmark 30 orange
      ]
        |> row
            [ width fill
            , spaceEvenly
            , Background.color black
            , Font.color white
            , padding 15
            ]
    , Input.button
        [ View.Attrs.sansSerifFont
        , padding 10
        , slightRound
        , Background.color Theme.orange
        , Font.bold
        , Font.color white
        , Font.size 20
        , width fill
        , hover
        ]
        { onPress = Just Types.ComposeToggle
        , label = text "Comment..."
        }
    ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            ]


viewPosts : DisplayProfile -> Bool -> Bool -> String -> Dict Int Time.Posix -> Time.Posix -> Wallet -> PublishedPostsDict -> Element Msg
viewPosts dProfile donateChecked showAddressOnPhace topic blockTimes now wallet posts =
    let
        state =
            { showAddress = False
            , showInput = Types.None
            }
    in
    posts
        |> Dict.values
        |> List.concat
        |> List.filter (isTopicMatch topic)
        |> List.map
            (View.Post.view
                dProfile
                showAddressOnPhace
                donateChecked
                blockTimes
                now
                wallet
                --(Wallet.unlockStatus wallet)
                --(Maybe.withDefault None inputState)
                state
            )
        |> column
            [ width fill
            , Background.color theme.blockBackground
            , Border.width 1
            , Border.color theme.blockBorderColor
            , Border.rounded 5
            ]


isTopicMatch : String -> Published -> Bool
isTopicMatch topicToFind post =
    case post.core.metadata.context of
        TopLevel postTopic ->
            postTopic == topicToFind

        Reply _ ->
            False
