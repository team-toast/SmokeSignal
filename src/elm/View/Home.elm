module View.Home exposing (view)

import Array
import Dict
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, white)
import Misc exposing (sortTypeToString)
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
            [ sortTypeUX model.sortType
            , posts
                |> List.map (viewPost model (Wallet.userInfo model.wallet))
                |> column
                    [ width fill
                    , height fill
                    , spacing 5
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
    , sortTypeUX model.sortType
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


sortTypeUX : SortType -> Element Msg
sortTypeUX activeSortType =
    Element.row
        [ Element.spacing 10
        , Element.paddingXY 10 5
        , Border.rounded 5
        , Background.color black
        , Font.color white
        ]
        [ Element.text "Sort by"
        , Element.row
            [ Element.spacing 5
            ]
            ([ BurnSort, HotSort, NewSort ]
                |> List.map
                    (\sortType ->
                        sortTypeButton sortType (sortType == activeSortType)
                    )
            )
        ]


sortTypeButton : SortType -> Bool -> Element Msg
sortTypeButton sortType isSelected =
    let
        dynamicAttributes =
            if isSelected then
                [ Background.color Theme.blue
                , Border.color black
                ]

            else
                [ Border.color Theme.blue ]
    in
    Input.button
        (dynamicAttributes
            ++ [ Font.semiBold
               , Element.paddingXY 10 5
               , Border.rounded 3
               , Border.width 1
               ]
        )
        { onPress = Just <| SetSortType sortType
        , label =
            sortType
                |> sortTypeToString
                |> Element.text
        }


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
        model.tooltipState
        (Just post.topic)
        wallet
        post.core
