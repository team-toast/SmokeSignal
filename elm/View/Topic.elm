module View.Topic exposing (view)

import Dict exposing (Dict)
import Element exposing (Element, centerX, column, fill, padding, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font
import Helpers.Element as EH exposing (DisplayProfile)
import Theme exposing (theme)
import Time
import Types exposing (Context(..), Model, Msg, Published, PublishedPostsDict, Wallet)
import View.Attrs exposing (cappedWidth, whiteGlowAttribute)
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
            , spacing 20
            , Element.paddingEach
                { top = 20
                , bottom = 0
                , right = 0
                , left = 0
                }
            ]


topicHeader : String -> Element Msg
topicHeader topic =
    column
        [ whiteGlowAttribute
        , Element.Font.color EH.white
        , width fill
        , padding 10
        ]
        [ text topic ]


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
            [ cappedWidth 600
            , centerX
            , Element.Background.color theme.blockBackground
            , Element.Border.width 1
            , Element.Border.color theme.blockBorderColor
            , Element.Border.rounded 5
            ]


isTopicMatch : String -> Published -> Bool
isTopicMatch topicToFind post =
    case post.core.metadata.context of
        TopLevel postTopic ->
            postTopic == topicToFind

        Reply _ ->
            False
