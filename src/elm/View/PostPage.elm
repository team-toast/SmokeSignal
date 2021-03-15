module View.PostPage exposing (view)

import Chain
import Dict
import Element exposing (Color, Element, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, white)
import Maybe.Extra exposing (unwrap)
import Misc
import Set
import Theme
import TokenValue
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, whiteGlowAttributeSmall)
import View.Common exposing (phaceElement, when)
import View.Img
import View.Markdown
import View.Post
import Wallet


view : Model -> Core -> Element Msg
view model post =
    let
        isMobile =
            model.dProfile == Mobile

        fontSize =
            if isMobile then
                25

            else
                50

        pd =
            if isMobile then
                15

            else
                30

        walletActive =
            model.wallet
                |> Wallet.isActive

        accounting =
            model.accounting
                |> Dict.get post.key
                |> View.Common.whenJust (viewAccounting model.dProfile)

        showActions =
            model.wallet
                |> Wallet.userInfo
                |> unwrap False (.chain >> (==) post.chain)
    in
    [ [ post.content.title
            |> View.Common.whenJust
                (text
                    >> List.singleton
                    >> Element.paragraph
                        [ Font.size fontSize
                        , Font.bold
                        ]
                )
      , [ [ accounting
          , phaceElement
                70
                post.author
                (model.showAddressId == Just (PhaceForPublishedPost post.id))
                (ShowOrHideAddress <| PhaceForPublishedPost post.id)
          , View.Post.viewCard post
          ]
            |> row [ spacing 10 ]
        , [ model.blockTimes
                |> Dict.get post.id.block
                |> View.Common.viewTiming model.now
          , Element.newTabLink [ hover ]
                { url = Chain.txUrl post.chain post.txHash
                , label =
                    [ View.Img.globe 20 white, text "View on block explorer" ]
                        |> row [ spacing 5, Font.underline ]
                }
          ]
            |> column
                [ spacing 10
                ]
        ]
            |> (if isMobile then
                    column [ spacing 10 ]

                else
                    row [ width fill, Element.spaceEvenly ]
               )
      , View.Common.horizontalRule white
      , post.content.body
            |> View.Markdown.renderString model.dProfile
            |> el
                [ width fill
                , height fill
                , Font.color white
                ]
      , [ Input.button
            [ Background.color Theme.orange
            , padding 10
            , roundBorder
            , hover
            , Font.color black
            , Element.alignBottom
            ]
            { onPress = Just ComposeOpen
            , label =
                [ View.Img.replyArrow 15 black
                , text "Reply"
                ]
                    |> row [ spacing 10, Font.size 20 ]
            }
            |> when walletActive
        , View.Post.viewActions post model.postState
            |> when showActions
        ]
            |> row [ spacing 10, Element.alignRight ]
      ]
        |> column
            [ spacing 20
            , width fill
            , padding pd
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            ]
    , model.replyIds
        |> Dict.get post.key
        |> unwrap [] Set.toList
        |> List.filterMap
            (\id ->
                Dict.get id model.replyPosts
            )
        |> List.sortBy (.core >> Misc.sortPosts model.blockTimes model.accounting model.now)
        |> List.map
            (\reply ->
                View.Post.view
                    model.dProfile
                    (model.blockTimes
                        |> Dict.get reply.core.id.block
                    )
                    model.now
                    (model.replyIds
                        |> Dict.get reply.core.key
                        |> Maybe.withDefault Set.empty
                    )
                    (model.accounting
                        |> Dict.get reply.core.key
                    )
                    (model.postState
                        |> Maybe.andThen
                            (\x ->
                                if x.id == reply.core.id then
                                    Just x

                                else
                                    Nothing
                            )
                    )
                    Nothing
                    (Wallet.userInfo model.wallet)
                    reply.core
            )
        |> column
            [ width fill
            , spacing 10
            , Element.paddingEach
                { left = 50
                , right = 0
                , top = 0
                , bottom = 0
                }
            ]
    ]
        |> column
            [ height fill
            , spacing 20
            , width fill
            , sansSerifFont
            ]


viewAccounting : DisplayProfile -> Accounting -> Element Msg
viewAccounting _ accounting =
    [ viewAmount Theme.darkRed accounting.totalBurned
    , viewAmount Theme.darkGreen accounting.totalTipped
    ]
        |> column
            [ spacing 5
            , height fill
            ]


viewAmount : Color -> TokenValue.TokenValue -> Element Msg
viewAmount color amount =
    [ View.Img.dollar 22 white
    , Misc.formatDollar amount
        |> text
    ]
        |> row
            [ padding 3
            , Border.rounded 3
            , Font.size 22
            , Font.color white
            , Background.color color
            , width fill
            , height fill
            ]
