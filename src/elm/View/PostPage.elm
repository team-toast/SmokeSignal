module View.PostPage exposing (view)

import Dict
import Element exposing (Element, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, white)
import Helpers.Time
import Maybe.Extra exposing (unwrap)
import Misc
import Set
import Theme
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, whiteGlowAttributeSmall)
import View.Common
import View.Img
import View.Markdown
import View.Post
import Wallet


view : Model -> Core -> Element Msg
view model post =
    [ [ [ [ text (post.content.title |> Maybe.withDefault ". . .") ]
            |> Element.paragraph
                [ Font.size 50
                ]
        , [ model.blockTimes
                |> Dict.get post.id.block
                |> View.Common.whenJust
                    (\time ->
                        Helpers.Time.sub model.now time
                            |> Helpers.Time.roundToSingleUnit
                            |> (\s -> s ++ " ago")
                            |> text
                    )
          , Element.newTabLink [ hover ]
                { url = Misc.txUrl post.chain post.txHash
                , label = text "View on Etherscan 🌐"
                }
          ]
            |> row
                [ width fill
                , Element.spaceEvenly
                ]
        ]
            |> column
                [ spacing 10
                , width fill
                , padding 10
                , whiteGlowAttributeSmall
                , Background.color black
                , Font.color white
                ]
      , Input.button [ Background.color Theme.orange, padding 20, roundBorder, hover ]
            { onPress = Just GoBack
            , label = text "Go back"
            }
      ]
        |> row
            [ spacing 10
            , width fill
            ]
    , [ post.content.body
            |> View.Markdown.renderString
            |> el
                [ width fill
                , height fill
                , Font.color white
                , Element.scrollbarY
                ]
      , Input.button
            [ Background.color Theme.orange
            , padding 10
            , roundBorder
            , hover
            , Element.alignRight
            ]
            { onPress = Just ComposeOpen
            , label =
                [ View.Img.replyArrow 15 black
                , text "Reply"
                ]
                    |> row [ spacing 10, Font.size 20 ]
            }
      ]
        |> column
            [ width fill
            , View.Attrs.cappedHeight 500
            , padding 10
            , spacing 10
            , whiteGlowAttributeSmall
            , Background.color black
            ]
    , model.replyIds
        |> Dict.get post.key
        |> unwrap [] Set.toList
        |> List.filterMap
            (\id ->
                Dict.get id model.replyPosts
            )
        |> List.sortBy (.core >> Misc.sortPosts model.blockTimes model.now)
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
                    (model.tipOpen
                        |> Maybe.andThen
                            (\x ->
                                if x.id == reply.core.id then
                                    Just x.showInput

                                else
                                    Nothing
                            )
                    )
                    model.compose.dollar
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
