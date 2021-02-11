module View.Modal exposing (viewNewToSmokeSignal)

import Element exposing (Attribute, Element, centerX, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile(..), white)
import Theme
import Types exposing (..)
import View.Attrs exposing (hover, whiteGlowAttribute)


viewNewToSmokeSignal : DisplayProfile -> Element Msg
viewNewToSmokeSignal dProfile =
    [ Element.text "Welcome to" |> rowElement dProfile []
    , rowElement
        dProfile
        []
      <|
        Element.image
            [ width <| Element.px 100
            ]
            { src = "./img/smokesignal-logo-vertical.svg"
            , description =
                "smokesignal logo"
            }
    , rowElement
        dProfile
        []
      <|
        Element.text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat."
    , rowElement
        dProfile
        []
      <|
        el
            [ Font.color Theme.orange, Font.size 28, Font.bold ]
        <|
            Element.text "No Usernames. No Moderators. No censorship. No Deplatforming."
    , rowElement
        dProfile
        []
      <|
        column
            [ Element.spacing 5 ]
            [ rowElement dProfile [] <| Element.text "All SmokeSignal posts are permanent and impossible for anyone to delete."
            , rowElement dProfile [] <| Element.text "All SmokeSignal authors are pseudonymous by default and cannot be deplatformed."
            ]
    , rowElement
        dProfile
        []
      <|
        column
            [ Element.spacing 5 ]
            [ rowElement dProfile [] <|
                Element.row
                    []
                    [ Element.text "All you need to post is a web3 wallet like "
                    , Element.newTabLink
                        [ Font.color <| Element.rgb 0.5 0.5 1 ]
                        { url = "https://metamask.io/"
                        , label = Element.text "Metamask"
                        }
                    , Element.text " and "
                    , Element.newTabLink
                        [ Font.color <| Element.rgb 0.5 0.5 1 ]
                        { url = "https://www.google.com/search?q=how+to+buy+ether"
                        , label = Element.text "ETH to burn."
                        }
                    ]
            , rowElement dProfile [] <| Element.text "The more ETH you burn, the more attention the post will get."
            ]
    , rowElement
        dProfile
        []
      <|
        Input.button
            [ Border.rounded 4
            , padding 10
            , Font.size 22
            , Font.bold
            , Border.glow
                (Element.rgb 0 0 1)
                3

            -- , Border.innerGlow
            --     (Element.rgb 0 0 1)
            --     2
            ]
            { onPress = Just <| ShowNewToSmokeSignalModal False
            , label = Element.text "Cool, let's go!"
            }
    ]
        |> column
            [ whiteGlowAttribute
            , Border.rounded 10
            , Font.color EH.white
            , width fill
            , height fill
            , Background.color <| Element.rgba 0 0 0 0.85
            , padding 50
            , Element.spacing 30
            , Input.button
                [ Element.alignTop
                , Element.alignRight
                , padding 30
                , Font.color white
                , Font.size 40
                , hover
                , View.Attrs.sansSerifFont
                ]
                { onPress = Just <| ShowNewToSmokeSignalModal False
                , label = text "X"
                }
                |> Element.inFront
            ]
        |> el [ Element.centerY, width fill, padding 30 ]


rowElement : DisplayProfile -> List (Attribute Msg) -> Element Msg -> Element Msg
rowElement _ attributes element =
    row
        ([ Element.height fill
         , Element.centerX
         ]
            ++ attributes
        )
        [ element ]
