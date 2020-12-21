module Common.View exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementMarkdown
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile)
import Helpers.Time as TimeHelpers
import Phace
import Post
import Routing exposing (Route)
import Theme exposing (theme)
import Time


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


web3ConnectButton : EH.DisplayProfile -> List (Attribute msg) -> (MsgUp -> msg) -> Element msg
web3ConnectButton dProfile attrs msgMapper =
    theme.emphasizedActionButton
        dProfile
        attrs
        [ "Connect to Wallet" ]
        (msgMapper ConnectToWeb3)


phaceElement : (Int, Int) -> Bool -> Address -> Bool -> msg -> msg -> Element msg
phaceElement (width, height) addressHangToRight fromAddress showAddress onClick noOpMsg =
    let
        addressOutputEl () =
            -- delay processing because addressToChecksumString is expensive!
            Element.el
                [ Element.alignBottom
                , if addressHangToRight then
                    Element.alignLeft

                  else
                    Element.alignRight
                , Element.Background.color EH.white
                , Element.Font.size 12
                , EH.moveToFront
                , Element.Border.width 2
                , Element.Border.color EH.black
                , EH.onClickNoPropagation noOpMsg
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
            [ Element.Border.rounded 5
            , Element.clip
            , Element.pointer
            , EH.onClickNoPropagation onClick
            -- , Element.Border.width 1
            -- , Element.Border.color Theme.blue
            ]
        <|
            Element.html
                (Phace.fromEthAddress fromAddress width height)


loadingElement : List (Attribute msg) -> Maybe String -> Element msg
loadingElement attrs maybeString =
    Element.el
        ([ Element.Font.italic
         , Element.Font.color theme.loadingTextColor
         , Element.Font.size 20
         ]
            ++ attrs
        )
        (Element.text <| Maybe.withDefault "loading..." maybeString)


emphasizedText : String -> Element msg
emphasizedText =
    Element.el
        [ Element.Font.bold
        , Element.Font.color EH.white
        ]
        << Element.text


daiSymbol : Bool -> List (Attribute msg) -> Element msg
daiSymbol isWhite attributes =
    Element.image attributes
        { src =
            if isWhite then
                "img/dai-unit-char-white.svg"

            else
                "img/dai-unit-char-black.svg"
        , description = ""
        }


appStatusMessage : Element.Color -> String -> Element msg
appStatusMessage color errStr =
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.paragraph
            [ Element.centerX
            , Element.centerY
            , Element.Font.center
            , Element.Font.italic
            , Element.Font.color color
            , Element.Font.size 36
            , Element.width (Element.fill |> Element.maximum 800)
            , Element.padding 40
            ]
            [ Element.text errStr ]


posixToString : Time.Posix -> String
posixToString t =
    let
        z =
            Time.utc
    in
    String.fromInt (Time.toYear z t)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt <| TimeHelpers.monthToInt <| Time.toMonth z t)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toDay z t))
        ++ " "
        ++ String.padLeft 2 '0' (String.fromInt (Time.toHour z t))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute z t))
        ++ " (UTC)"


subheaderAttributes : DisplayProfile -> List (Attribute msg)
subheaderAttributes dProfile =
    [ Element.paddingXY 0 (responsiveVal dProfile 20 10)
    , Element.Font.size (responsiveVal dProfile 50 30)
    , Element.Font.color theme.headerTextColor
    ]


commonFontSize : DisplayProfile -> Int
commonFontSize dProfile =
    case dProfile of
        Desktop ->
            24

        Mobile ->
            18


viewMetadata : Bool -> Post.Metadata -> Element MsgUp
viewMetadata showContext metadata =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ case metadata.maybeDecodeError of
            Just jsonDecodeErr ->
                viewMetadataDecodeError jsonDecodeErr

            Nothing ->
                Element.none
        , if showContext then
            Element.el [ Element.alignLeft ] <|
                viewContext metadata.context

          else
            Element.none
        ]


viewMetadataDecodeError : String -> Element msg
viewMetadataDecodeError error =
    Element.el
        [ Element.Border.rounded 5
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.3
        , Element.clip
        ]
    <|
        Element.el
            [ Element.Font.color theme.errorTextColor
            , Element.Font.italic
            , Element.Font.size 18
            , Element.height (Element.shrink |> Element.maximum 80)
            , Element.width (Element.shrink |> Element.maximum 400)
            , Element.scrollbars
            , Element.Background.color <| Element.rgba 1 0 0 0.1
            ]
            (Element.text <|
                "Metadata decode error:\n\n"
                    ++ error
            )


viewContext : Post.Context -> Element MsgUp
viewContext context =
    case context of
        Post.ForPost postId ->
            viewReplyInfo postId

        Post.ForTopic topic ->
            viewTopic topic


viewTopic : String -> Element MsgUp
viewTopic topic =
    Element.column
        [ Element.padding 10
        , Element.Border.rounded 5
        , Element.Font.size 20
        , Element.Font.italic
        , Element.Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        , Element.clipX
        , Element.scrollbarX
        , Element.width (Element.shrink |> Element.maximum 400)
        ]
        [ Element.text "Topic:"
        , Element.el
            [ Element.Font.color theme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                GotoRoute <|
                    Routing.ViewContext <|
                        Post.ForTopic topic
            ]
            (Element.text topic)
        ]


viewReplyInfo : Post.Id -> Element MsgUp
viewReplyInfo postId =
    Element.row
        [ Element.padding 10
        , Element.Border.rounded 5
        , Element.Font.size 20
        , Element.Font.italic
        , Element.Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        ]
        [ Element.column
            [ Element.spacing 3
            ]
            [ Element.text "Replying to:"
            , Element.el
                [ Element.Font.color theme.linkTextColor
                , Element.pointer
                , Element.Events.onClick <|
                    GotoRoute <|
                        Routing.ViewContext <|
                            Post.ForPost postId
                ]
                (Element.text <|
                    shortenedHash postId.messageHash
                )
            ]
        ]


coloredAppTitle : List (Attribute msg) -> Element msg
coloredAppTitle attributes =
    Element.row attributes
        [ Element.el [ Element.Font.color Theme.darkGray ] <| Element.text "Smoke"
        , Element.el [ Element.Font.color <| Element.rgb 1 0.5 0 ] <| Element.text "Signal"
        ]


maxContentColWidth =
    1000


renderContentOrError : String -> Element msg
renderContentOrError content =
    let
        renderResult =
            ElementMarkdown.renderString
                [ Element.spacing 15
                , Element.Font.color theme.postBodyTextColor
                , Element.width Element.fill
                ]
                content
    in
    case renderResult of
        Ok rendered ->
            rendered

        Err errStr ->
            Element.el
                [ Element.Font.color theme.errorTextColor
                , Element.Font.italic
                ]
            <|
                Element.text <|
                    "Error parsing/rendering markdown: "
                        ++ errStr


unlockUXOr : DisplayProfile -> List (Attribute msg) -> UnlockStatus -> (MsgUp -> msg) -> Element msg -> Element msg
unlockUXOr dProfile attributes unlockStatus msgMapper el =
    case unlockStatus of
        NotConnected ->
            web3ConnectButton
                dProfile
                attributes
                msgMapper

        Checking ->
            loadingElement
                attributes
            <|
                Just "Checking DAI lock..."

        Locked ->
            unlockButton
                dProfile
                attributes
                msgMapper

        Unlocking ->
            loadingElement
                attributes
            <|
                Just "Unlocking DAI..."

        Unlocked ->
            Element.el attributes el


unlockButton : EH.DisplayProfile -> List (Attribute msg) -> (MsgUp -> msg) -> Element msg
unlockButton dProfile attrs msgMapper =
    theme.emphasizedActionButton
        dProfile
        attrs
        [ "Unlock Dai" ]
        (msgMapper UnlockDai)


daiAmountInput : DisplayProfile -> List (Attribute msg) -> String -> (String -> msg) -> Element msg
daiAmountInput dProfile attributes currentInput onChange =
    Element.Input.text
        [ Element.width <| Element.px (100 |> changeForMobile 60 dProfile)
        , Element.height <| Element.px (40 |> changeForMobile 35 dProfile)
        , Element.Font.size (20 |> changeForMobile 14 dProfile)
        , Element.Background.color <| Element.rgba 1 1 1 0.4
        ]
        { onChange = onChange
        , text = currentInput
        , placeholder = Nothing
        , label = Element.Input.labelHidden "dai amount"
        }
