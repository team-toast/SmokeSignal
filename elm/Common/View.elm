module Common.View exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Helpers.Element as EH
import Phace
import Theme exposing (..)


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


web3ConnectButton : EH.DisplayProfile -> List (Attribute MsgUp) -> Element MsgUp
web3ConnectButton dProfile attrs =
    redButton
        dProfile
        attrs
        [ "Connect to Wallet" ]
        ConnectToWeb3


phaceElement : PhaceIconId -> Address -> Bool -> Element MsgUp
phaceElement phaceId fromAddress showAddress =
    let
        addressOutputEl =
            Element.el
                [ Element.alignBottom
                , Element.alignLeft
                , Element.Border.width 2
                , Element.Border.color EH.black
                , Element.Background.color EH.white
                , Element.Font.size 12
                , EH.moveToFront
                ]
                (Element.text <| Eth.Utils.addressToChecksumString fromAddress)
    in
    Element.el
        (if showAddress then
            [ Element.inFront addressOutputEl
            , Element.alignTop
            ]

         else
            [ Element.alignTop ]
        )
    <|
        Element.el
            [ Element.Border.rounded 10
            , Element.clip
            , Element.Border.width 2
            , Element.Border.color EH.black
            , Element.pointer
            , EH.onClickNoPropagation (ShowOrHideAddress phaceId)
            ]
        <|
            Element.html
                (Phace.fromEthAddress fromAddress)


loadingElement : List (Attribute msg) -> Maybe String -> Element msg
loadingElement attrs maybeString =
    Element.el
        ([ Element.Font.italic
         , Element.Font.color darkGray
         , Element.Font.size 20
         ]
            ++ attrs
        )
        (Element.text <| Maybe.withDefault "loading..." maybeString)
