module View.Common exposing (daiAmountInput, daiSymbol, phaceElement, renderContentOrError, unlockButton, unlockUXOr, whiteGlowAttribute, whiteGlowAttributeSmall)

import Element exposing (Attribute, Element)
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
import Types exposing (Content, Msg, UnlockStatus(..))


web3ConnectButton : EH.DisplayProfile -> List (Attribute Msg) -> Element Msg
web3ConnectButton dProfile attrs =
    theme.emphasizedActionButton
        dProfile
        attrs
        [ "Connect to Wallet" ]
        (EH.Action Types.ConnectToWeb3)


loadingElement : List (Attribute Msg) -> Maybe String -> Element Msg
loadingElement attrs maybeString =
    Element.el
        ([ Font.italic
         , Font.color theme.loadingTextColor
         , Font.size 20
         ]
            ++ attrs
        )
        (Element.text <| Maybe.withDefault "loading..." maybeString)


whiteGlowAttribute : Element.Attribute Msg
whiteGlowAttribute =
    Border.glow
        (Element.rgba 1 1 1 0.4)
        5


whiteGlowAttributeSmall : Element.Attribute Msg
whiteGlowAttributeSmall =
    Border.glow
        (Element.rgba 1 1 1 0.4)
        2


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


daiAmountInput : DisplayProfile -> List (Attribute Msg) -> String -> (String -> Msg) -> Element Msg
daiAmountInput dProfile attributes currentInput onChange =
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


unlockUXOr : DisplayProfile -> List (Attribute Msg) -> UnlockStatus -> Element Msg -> Element Msg
unlockUXOr dProfile attributes unlockStatus el =
    case unlockStatus of
        NotConnected ->
            web3ConnectButton
                dProfile
                attributes

        Checking ->
            loadingElement
                attributes
            <|
                Just "Checking DAI lock..."

        Locked ->
            unlockButton
                dProfile
                attributes

        Unlocking ->
            loadingElement
                attributes
            <|
                Just "Unlocking DAI..."

        Unlocked ->
            Element.el attributes el


unlockButton : EH.DisplayProfile -> List (Attribute Msg) -> Element Msg
unlockButton dProfile attrs =
    theme.emphasizedActionButton
        dProfile
        attrs
        [ "Unlock Dai" ]
        (EH.Action Types.UnlockDai)
