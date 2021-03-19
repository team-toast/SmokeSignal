module View.Modal exposing (viewCookieConsent, viewNewToSmokeSignal)

import Element exposing (Element, centerX, column, el, fill, height, padding, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile(..), white)
import Theme exposing (blue)
import Types exposing (..)
import View.Attrs exposing (cappedWidth, hover, roundBorder, sansSerifFont, whiteGlowAttribute)
import View.Common
import View.Img


viewNewToSmokeSignal : DisplayProfile -> Element Msg
viewNewToSmokeSignal _ =
    [ text "Welcome to"
        |> el [ centerX, Font.size 30 ]
    , View.Img.logo 120
        |> el [ centerX ]
    , text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat."
        |> el [ centerX ]
    , text "No Usernames. No Moderators. No censorship. No Deplatforming."
        |> el [ Font.color Theme.orange, Font.size 28, Font.bold ]
    , [ text "All SmokeSignal posts are permanent and impossible for anyone to delete."
            |> el [ centerX ]
      , text "All SmokeSignal authors are pseudonymous by default and cannot be deplatformed."
            |> el [ centerX ]
      ]
        |> column [ spacing 5, width fill ]
    , [ [ text "All you need to post is a web3 wallet like "
        , Element.newTabLink
            [ Font.color <| Element.rgb 0.5 0.5 1 ]
            { url = "https://metamask.io/"
            , label = text "MetaMask"
            }
        , text " and "
        , Element.newTabLink
            [ Font.color <| Element.rgb 0.5 0.5 1 ]
            { url = "https://www.google.com/search?q=how+to+buy+ether"
            , label = text "ETH to burn."
            }
        ]
            |> row [ centerX ]
      , text "The more ETH you burn, the more attention the post will get."
            |> el [ centerX ]
      ]
        |> column [ spacing 5, width fill ]
    , Input.button
        [ Border.rounded 4
        , padding 10
        , Font.size 22
        , Font.bold
        , centerX
        , Border.glow (Element.rgb 0 0 1) 3
        ]
        { onPress = Just <| ShowNewToSmokeSignalModal False
        , label = text "Cool, let's go!"
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
            , spacing 30
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
        |> View.Common.wrapModal (ShowNewToSmokeSignalModal False)


viewCookieConsent : Element Msg
viewCookieConsent =
    [ [ Element.newTabLink [ Font.bold, hover ]
            { url = "https://foundrydao.com/"
            , label = text "Foundry"
            }
      , text " products use cookies and analytics to track behavior patterns, to help zero in on effective marketing strategies. To avoid being tracked in this way, we recommend using the "
      , Element.newTabLink [ Font.bold, hover ]
            { url = "https://brave.com/"
            , label = text "Brave browser"
            }
      , text " or installing the "
      , Element.newTabLink [ Font.bold, hover ]
            { url = "https://tools.google.com/dlpage/gaoptout"
            , label = text "Google Analytics Opt-Out browser addon"
            }
      , text "."
      ]
        |> paragraph [ Font.color white, sansSerifFont ]
    , Input.button [ Background.color Theme.orange, padding 20, roundBorder, hover ]
        { onPress = Just CookieConsentGranted
        , label = text "Understood"
        }
    ]
        |> row
            [ Background.color blue
            , Element.alignBottom
            , cappedWidth 900
            , centerX
            , sansSerifFont
            , padding 20
            , spacing 20
            , Border.roundEach
                { bottomLeft = 0
                , bottomRight = 0
                , topLeft = 20
                , topRight = 20
                }
            ]
