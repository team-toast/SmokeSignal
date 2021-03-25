module View.Topic exposing (view)

import Dict
import Element exposing (Element, centerX, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (black, white)
import Misc
import Set
import Theme exposing (orange)
import Types exposing (..)
import View.Attrs exposing (hover, slightRound, whiteGlowAttributeSmall)
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
                        (model.postState
                            |> Maybe.andThen
                                (\x ->
                                    if x.id == post.core.id then
                                        Just x

                                    else
                                        Nothing
                                )
                        )
                        model.tooltipState
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
    [ [ text <| "#" ++ topic
      ]
        |> Element.paragraph
            [ Font.color black
            , Font.size 35
            ]
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
        { onPress = Just Types.ComposeOpen
        , label =
            [ text "+" |> el [ Font.size 30 ], text "New Post" ]
                |> row [ spacing 5 ]
        }
    ]
        |> row
            [ width fill
            , whiteGlowAttributeSmall
            , padding 15
            , Background.color orange
            , Font.color orange
            ]
