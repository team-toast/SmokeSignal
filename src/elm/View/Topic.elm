module View.Topic exposing (view)

import Chain
import Dict
import Element exposing (Element, centerX, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Misc
import Theme exposing (black, orange, white)
import Types exposing (..)
import View.Attrs exposing (hover, slightRound, whiteGlowAttributeSmall)
import View.Common
import View.Post
import Wallet


view : Model -> String -> Element Msg
view model topic =
    let
        posts =
            model.rootPosts
                |> Dict.values
                |> List.filter (.topic >> (==) topic)
                |> List.sortBy
                    (.core
                        >> Misc.sortPostsFunc
                            model.sortType
                            model.blockTimes
                            model.accounting
                            model.now
                    )
    in
    [ topicHeader topic
    , if List.isEmpty posts then
        text "Be the first to create a post on this topic."
            |> el
                [ padding 10
                , whiteGlowAttributeSmall
                , Background.color black
                , Font.color white
                , centerX
                ]

      else
        posts
            |> List.map
                (\post ->
                    View.Post.view
                        model.dProfile
                        (model.blockTimes
                            |> Dict.get ( Chain.getName post.core.chain, post.core.id.block )
                        )
                        model.now
                        model.replyIds
                        model.accounting
                        model.maybeBurnOrTipUX
                        model.maybeActiveTooltip
                        (Just topic)
                        (Wallet.userInfo model.wallet)
                        post.core
                )
            |> column
                [ width fill
                , spacing 10
                ]
    ]
        |> column
            [ width fill
            , height fill
            , spacing 10
            , Element.alignTop
            ]


topicHeader : String -> Element Msg
topicHeader topic =
    [ [ View.Common.topic topic ]
        |> Element.paragraph []
    , Input.button
        [ View.Attrs.sansSerifFont
        , padding 10
        , slightRound
        , Background.color Theme.orange
        , Font.bold
        , Font.color black
        , Font.size 20
        , hover
        ]
        { onPress = Just <| GotoView ViewCompose
        , label =
            [ text "+" |> el [ Font.size 30 ], text "New Post" ]
                |> row [ spacing 5 ]
        }
    ]
        |> row
            [ width fill
            , whiteGlowAttributeSmall
            , padding 15
            , Font.size 35
            , Background.color black
            , Font.color orange
            ]
