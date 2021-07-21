module View.Home exposing (view)

import Array
import Chain
import Dict
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Misc exposing (sortTypeToString)
import Theme exposing (black, orange, white)
import Types exposing (..)
import View.Attrs exposing (hover, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (whenAttr)
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
            , pages
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
    , pages
    , posts
        |> List.map (viewPost model (Wallet.userInfo model.wallet))
        |> column
            [ width fill
            , height fill
            , spacing 20
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
    Input.button
        [ Font.semiBold
        , Element.paddingXY 10 5
        , Border.rounded 3
        , Border.width 1
        , hover
        , Border.color Theme.blue
        , Background.color Theme.blue
            |> whenAttr isSelected
        ]
        { onPress = Just <| SetSortType sortType
        , label =
            sortType
                |> sortTypeToString
                |> Element.text
        }


type PaginationViewElement
    = PageNum Int
    | Break


viewPaginationElement : DisplayProfile -> Int -> PaginationViewElement -> Element Msg
viewPaginationElement dProfile activePage paginationElement =
    case paginationElement of
        PageNum num ->
            Input.button
                [ Background.color
                    (if num == activePage then
                        Theme.orange

                     else
                        white
                    )
                , width <| px <| Misc.responsiveVal dProfile 50 30
                , height <| px <| Misc.responsiveVal dProfile 50 30
                , Border.rounded 25
                , View.Attrs.sansSerifFont
                , hover
                , Font.size 30
                ]
                { onPress = Just <| SetPage num
                , label =
                    (num + 1)
                        |> String.fromInt
                        |> text
                        |> el [ centerX, centerY ]
                }

        Break ->
            row [ spacing 2 ]
                (List.repeat 3 <|
                    el
                        [ width <| px <| Misc.responsiveVal dProfile 8 6
                        , height <| px <| Misc.responsiveVal dProfile 8 6
                        , Border.rounded <| Misc.responsiveVal dProfile 4 3
                        , Background.color white
                        ]
                        Element.none
                )


viewPagination : Model -> Element Msg
viewPagination model =
    let
        numButtonsShownInRange =
            Misc.responsiveVal model.dProfile 7 3

        paginationViewElements =
            (if model.currentPage - (numButtonsShownInRange // 2) <= 1 then
                List.range 0 (model.currentPage - 1)
                    |> List.map PageNum

             else
                [ PageNum 0, Break ]
                    ++ (List.range (model.currentPage - (numButtonsShownInRange // 2)) (model.currentPage - 1)
                            |> List.map PageNum
                       )
            )
                ++ [ PageNum model.currentPage ]
                ++ (if model.currentPage + (numButtonsShownInRange // 2) >= Array.length model.pages - 2 then
                        List.range (model.currentPage + 1) (Array.length model.pages - 1)
                            |> List.map PageNum

                    else
                        (List.range (model.currentPage + 1) (model.currentPage + (numButtonsShownInRange // 2))
                            |> List.map PageNum
                        )
                            ++ [ Break, PageNum (Array.length model.pages - 1) ]
                   )
    in
    row
        [ centerX
        , height <| px 50
        , spacing <| Misc.responsiveVal model.dProfile 20 10
        ]
        (paginationViewElements
            |> List.map (viewPaginationElement model.dProfile model.currentPage)
        )


viewPost : Model -> Maybe UserInfo -> RootPost -> Element Msg
viewPost model wallet post =
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
        (Just post.topic)
        wallet
        post.core
