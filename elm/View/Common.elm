module View.Common exposing (appStatusMessage, daiAmountInput, daiSymbol, phaceElement, renderContentOrError, shortenedHash, unlockButton, viewContext, web3ConnectButton, whenAttr, whenJust, wrapModal)

{-| A module for managing elm-ui 'Element' helper functions and reuseable components.
-}

import Element exposing (Attribute, Element, column, fill, height, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ElementMarkdown
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, responsiveVal)
import Phace
import Theme exposing (theme)
import Types exposing (Content, Context, Id, Msg)


viewContext : Context -> Element Msg
viewContext context =
    case context of
        Types.Reply postId ->
            viewReplyInfo postId

        Types.TopLevel topic ->
            viewTopic topic


viewTopic : String -> Element Msg
viewTopic topic =
    Element.column
        [ Element.padding 10
        , Border.rounded 5
        , Font.size 20
        , Font.italic
        , Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        , Element.clipX
        , Element.scrollbarX
        , Element.width (Element.shrink |> Element.maximum 400)
        ]
        [ Element.text "Topic:"
        , Element.el
            [ Font.color theme.linkTextColor
            , Element.pointer

            --, Element.Events.onClick <|
            --GotoRoute <|
            --RouteViewContext <|
            --Topic topic
            ]
            (Element.text topic)
        ]


viewReplyInfo : Id -> Element Msg
viewReplyInfo postId =
    Element.row
        [ Element.padding 10
        , Border.rounded 5
        , Font.size 20
        , Font.italic
        , Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        ]
        [ Element.column
            [ Element.spacing 3
            ]
            [ Element.text "Replying to:"
            , Element.el
                [ Font.color theme.linkTextColor
                , Element.pointer

                --, Element.Events.onClick <|
                --GotoRoute <|
                --RouteViewContext <|
                --Types.ViewPost postId
                ]
                (Element.text <|
                    shortenedHash postId.messageHash
                )
            ]
        ]


shortenedHash : Hex -> String
shortenedHash hash =
    let
        hashStr =
            Eth.Utils.hexToString hash
    in
    if String.length hashStr <= 10 then
        hashStr

    else
        String.left 6 hashStr
            ++ "..."
            ++ String.right 4 hashStr


appStatusMessage : Element.Color -> String -> Element Msg
appStatusMessage color errStr =
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.paragraph
            [ Element.centerX
            , Element.centerY
            , Font.center
            , Font.italic
            , Font.color color
            , Font.size 36
            , Element.width (Element.fill |> Element.maximum 800)
            , Element.padding 40
            ]
            [ Element.text errStr ]


web3ConnectButton : EH.DisplayProfile -> List (Attribute Msg) -> Element Msg
web3ConnectButton dProfile attrs =
    theme.emphasizedActionButton
        dProfile
        attrs
        [ "Connect to Wallet" ]
        (EH.Action Types.ConnectToWeb3)


phaceElement : ( Int, Int ) -> Bool -> Address -> Bool -> Msg -> Element Msg
phaceElement ( width, height ) addressHangToRight fromAddress showAddress onClick =
    let
        addressOutputEl () =
            -- delay processing because addressToChecksumString is expensive!
            Element.el
                [ Element.alignBottom
                , if addressHangToRight then
                    Element.alignLeft

                  else
                    Element.alignRight
                , Background.color EH.white
                , Font.size 12
                , EH.moveToFront
                , Border.width 2
                , Border.color EH.black

                --, EH.onClickNoPropagation noOpMsg
                ]
                (Element.text <| Eth.Utils.addressToChecksumString fromAddress)
    in
    Element.el
        (if showAddress then
            [ Element.inFront <| addressOutputEl ()
            , Element.alignTop
            ]

         else
            [ Element.alignTop ]
        )
    <|
        Element.el
            [ Border.rounded 5
            , Element.clip
            , Element.pointer
            , EH.onClickNoPropagation onClick

            -- , Border.width 1
            -- , Border.color Theme.blue
            ]
        <|
            Element.html
                (Phace.fromEthAddress fromAddress width height)


daiSymbol : Bool -> List (Attribute Msg) -> Element Msg
daiSymbol isWhite attributes =
    Element.image attributes
        { src =
            if isWhite then
                "img/dai-unit-char-white.svg"

            else
                "img/dai-unit-char-black.svg"
        , description = ""
        }


daiAmountInput : DisplayProfile -> String -> (String -> Msg) -> Element Msg
daiAmountInput dProfile currentInput onChange =
    Input.text
        [ Element.width <| Element.px (responsiveVal dProfile 100 60)
        , Element.height <| Element.px (responsiveVal dProfile 40 35)
        , Font.size (responsiveVal dProfile 20 14)
        , Background.color <| Element.rgba 1 1 1 0.4
        ]
        { onChange = onChange
        , text = currentInput
        , placeholder = Nothing
        , label = Input.labelHidden "dai amount"
        }


renderContentOrError : Content -> Element Msg
renderContentOrError content =
    let
        renderResult =
            ElementMarkdown.renderString
                [ Element.spacing 15
                , Font.color theme.postBodyTextColor
                , Element.width Element.fill
                ]
                content.body
    in
    case renderResult of
        Ok rendered ->
            rendered

        Err errStr ->
            Element.el
                [ Font.color theme.errorTextColor
                , Font.italic
                ]
            <|
                Element.text <|
                    "Error parsing/rendering markdown: "
                        ++ errStr


unlockButton : EH.DisplayProfile -> List (Attribute Msg) -> Element Msg
unlockButton dProfile attrs =
    theme.emphasizedActionButton
        dProfile
        attrs
        [ "Unlock Dai" ]
        (EH.Action Types.UnlockDai)


whenJust : (a -> Element msg) -> Maybe a -> Element msg
whenJust fn =
    Maybe.map fn >> Maybe.withDefault Element.none


whenAttr : Bool -> Attribute msg -> Attribute msg
whenAttr bool =
    if bool then
        identity

    else
        Element.below Element.none
            |> always


{-| Wraps an element with transparent clickable areas.
-}
wrapModal : msg -> Element msg -> Element msg
wrapModal msg elem =
    let
        btn =
            Input.button
                [ height fill
                , width fill
                , Background.color <| Element.rgba255 0 0 0 0.4
                ]
                { onPress = Just msg
                , label = Element.none
                }
    in
    [ btn
    , [ btn, elem, btn ]
        |> column [ width fill, height fill ]
    , btn
    ]
        |> row [ width fill, height fill ]
