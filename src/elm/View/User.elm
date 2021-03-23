module View.User exposing (view)

import Dict
import Element exposing (Element, centerX, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element exposing (black, white)
import Misc
import Set
import Theme exposing (orange)
import Types exposing (..)
import View.Attrs exposing (whiteGlowAttributeSmall)
import View.Common
import View.Post
import Wallet


view : Model -> Address -> Element Msg
view model address =
    let
        posts =
            model.rootPosts
                |> Dict.values
                |> List.filter (.core >> .author >> (==) address)
                |> List.sortBy
                    (.core
                        >> Misc.sortPostsFunc
                            model.sortType
                            model.blockTimes
                            model.accounting
                            model.now
                    )
    in
    [ viewHeader address
    , if List.isEmpty posts then
        text "This account has not posted on SmokeSignal."
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
                        --(Just topic)
                        Nothing
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


viewHeader : Address -> Element Msg
viewHeader address =
    [ Eth.Utils.addressToString address
        |> View.Common.ellipsisText 35
        |> el
            [ Font.color black
            , Font.size 35
            , width fill
            ]
    ]
        |> row
            [ width fill
            , whiteGlowAttributeSmall
            , padding 15
            , Background.color orange
            , Font.color orange
            ]
