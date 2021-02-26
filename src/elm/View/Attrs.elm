module View.Attrs exposing (cappedHeight, cappedWidth, fade, hover, onEnter, onKeydown, rotate, roundBorder, sansSerifFont, slightRound, style, title, typeFont, whiteGlowAttribute, whiteGlowAttributeSmall)

{-| A module for managing elm-ui 'Attribute' values and related functions.
-}

import Element exposing (Attribute)
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)


title : String -> Attribute msg
title =
    Html.Attributes.title
        >> Element.htmlAttribute


style : String -> String -> Attribute msg
style k v =
    Html.Attributes.style k v
        |> Element.htmlAttribute


hover : Attribute msg
hover =
    Element.mouseOver [ fade ]


fade : Element.Attr a b
fade =
    Element.alpha 0.7


slightRound : Attribute msg
slightRound =
    Border.rounded 1


roundBorder : Attribute msg
roundBorder =
    Border.rounded 5


typeFont : Attribute msg
typeFont =
    Font.family
        [ Font.typeface "SmallType"
        ]


sansSerifFont : Attribute msg
sansSerifFont =
    Font.family
        [ Font.typeface "DDin"
        ]


cappedWidth : Int -> Attribute msg
cappedWidth n =
    Element.fill |> Element.maximum n |> Element.width


cappedHeight : Int -> Attribute msg
cappedHeight n =
    Element.fill |> Element.maximum n |> Element.height


whiteGlowAttribute : Attribute msg
whiteGlowAttribute =
    Border.glow
        (Element.rgba 1 1 1 0.4)
        5


whiteGlowAttributeSmall : Attribute msg
whiteGlowAttributeSmall =
    Border.glow
        (Element.rgba 1 1 1 0.4)
        2


onEnter : msg -> Decoder msg
onEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail ""
            )


onKeydown : List (Decoder msg) -> Attribute msg
onKeydown decoders =
    -- Can be replaced by a Decode.oneOf when this is fixed
    -- https://github.com/elm/json/issues/13
    Decode.value
        |> Decode.andThen
            (\val ->
                let
                    matchKeydown ds =
                        case ds of
                            decoder :: tail ->
                                val
                                    |> Decode.decodeValue decoder
                                    |> Result.map Decode.succeed
                                    |> Result.withDefault (matchKeydown tail)

                            [] ->
                                Decode.fail "No match"
                in
                matchKeydown decoders
            )
        |> Html.Events.on "keydown"
        |> Element.htmlAttribute


rotate : Attribute msg
rotate =
    style "animation" "rotation 0.7s infinite linear"
