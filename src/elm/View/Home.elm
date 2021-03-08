module View.Home exposing (view)

import Dict
import Element exposing (Element, centerX, column, el, fill, height, padding, paddingXY, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), white)
import Misc
import Set
import Theme exposing (orange)
import Types exposing (..)
import View.Attrs exposing (slightRound, whiteGlowAttributeSmall)
import View.Post
import Wallet


view : Model -> Element Msg
view model =
    case model.dProfile of
        Desktop ->
            viewDesktop model

        Mobile ->
            Dict.values model.rootPosts
                |> List.sortBy (.core >> Misc.sortPosts model.blockTimes model.now)
                |> List.map (viewPost model (Wallet.userInfo model.wallet))
                |> column
                    [ width fill
                    , height fill
                    , spacing 5
                    , padding 5
                    ]


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
    , posts
        |> List.sortBy (.core >> Misc.sortPosts model.blockTimes model.now)
        |> List.map (viewPost model (Wallet.userInfo model.wallet))
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
