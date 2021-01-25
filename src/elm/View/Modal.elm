module View.Modal exposing (viewNewToSmokeSignal)

import Element exposing (Attribute, Element, centerX, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile(..), white)
import Theme
import Types exposing (Context(..), Msg(..), PhaceIconId(..), Post(..), Route(..))
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
            { src = "img/smokesignal-logo-vertical.svg"
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
        Element.text "All you need is ETH for gas and DAI to burn."
    , rowElement
        dProfile
        []
      <|
        column
            [ Element.spacing 5 ]
            [ rowElement dProfile [] <| Element.text "All SmokeSignal posts are permanent and impossible to delete, and can be"
            , rowElement dProfile [] <| Element.text "accessed with any browser via an IPFS Gateway (example)"
            , rowElement dProfile [] <| Element.text "or the smokesignal.eth.link mirror (example)."
            ]
    , rowElement
        dProfile
        []
      <|
        column
            [ Element.spacing 5 ]
            [ rowElement dProfile [] <| Element.text "If the above two methods prove unreliable, some browsers also support direct"
            , rowElement dProfile [] <| Element.text "smokesignal.eth links (example) or direct IPFS links (example)."
            ]
    , rowElement
        dProfile
        []
      <|
        el
            [ Font.color Theme.orange
            , Font.semiBold
            ]
        <|
            Element.text
                "Go to introductory video →"
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
                { onPress = Just CloseNewToSmokeSignalModal
                , label = text "X"
                }
                |> Element.inFront
            ]
        |> el [ height fill, width fill, padding 30 ]


rowElement : DisplayProfile -> List (Attribute Msg) -> Element Msg -> Element Msg
rowElement _ attributes element =
    row
        ([ Element.height fill
         , Element.centerX
         ]
            ++ attributes
        )
        [ element ]
