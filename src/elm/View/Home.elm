module View.Home exposing (view)

import Array
import Dict
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), white)
import Set
import Theme exposing (orange)
import Types exposing (..)
import View.Attrs exposing (hover, slightRound, whiteGlowAttributeSmall)
import View.Post
import Wallet


view : Model -> Element Msg
view model =
    case model.dProfile of
        Desktop ->
            viewDesktop model

        Mobile ->
            let
                posts =
                    model.pages
                        |> Array.get model.currentPage
                        |> Maybe.withDefault []
                        |> List.filterMap
                            (\key ->
                                Dict.get key model.rootPosts
                            )

                pages =
                    viewPagination model
            in
            [ pages
            , posts
                |> List.map (viewPost model (Wallet.userInfo model.wallet))
                |> column
                    [ width fill
                    , height fill
                    , spacing 5
                    , padding 5
                    ]
            , pages
            ]
                |> column [ width fill, height fill, spacing 10 ]


viewDesktop : Model -> Element Msg
viewDesktop model =
    let
        posts =
            model.pages
                |> Array.get model.currentPage
                |> Maybe.withDefault []
                |> List.filterMap
                    (\key ->
                        Dict.get key model.rootPosts
                    )

        pages =
            viewPagination model
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
        , Element.mouseOver
            [ Background.color Theme.darkRed
            ]
        ]
        { onPress = Just <| ShowNewToSmokeSignalModal True
        , label =
            "NEW TO SMOKE SIGNAL?"
                |> text
                |> Element.el [ centerX ]
        }
        |> always Element.none
    , pages
    , posts
        |> List.map (viewPost model (Wallet.userInfo model.wallet))
        |> column
            [ width fill
            , height fill
            , spacing 5
            , paddingXY 0 5
            ]
    , pages
    ]
        |> column
            [ spacing 10
            , width fill
            , height fill
            ]


viewPagination : Model -> Element Msg
viewPagination model =
    List.range 0 (Array.length model.pages - 1)
        |> List.map
            (\n ->
                Input.button
                    [ Background.color
                        (if n == model.currentPage then
                            Theme.orange

                         else
                            white
                        )
                    , width <| px 50
                    , height <| px 50
                    , Border.rounded 25
                    , View.Attrs.sansSerifFont
                    , hover
                    , Font.size 30
                    ]
                    { onPress = Just <| SetPage n
                    , label =
                        (n + 1)
                            |> String.fromInt
                            |> text
                            |> el [ centerX, centerY ]
                    }
            )
        |> row [ spacing 20, centerX ]


viewPost : Model -> Maybe UserInfo -> RootPost -> Element Msg
viewPost model wallet post =
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
        (Just post.topic)
        wallet
        post.core
