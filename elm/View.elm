module View exposing (root)

import Browser
import CommonTypes exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Lazy
import ElementMarkdown
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Helpers.Time as TimeHelpers
import Html.Attributes
import Json.Decode
import List.Extra
import Maybe.Extra
import Message exposing (Message, blankVersionedMetadata)
import Phace
import Routing exposing (Route)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet


root : Model -> Browser.Document Msg
root model =
    { title = "SmokeSignal"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
            , Element.Events.onClick ClickHappened
            ]
          <|
            body model
        ]
    }


body : Model -> Element Msg
body model =
    Element.column
        ([ Element.width Element.fill
         , Element.height Element.fill
         ]
            ++ List.map
                Element.inFront
                (userNoticeEls
                    Desktop
                    model.userNotices
                )
        )
        [ header model.viewFilter
        , case model.viewFilter of
            None ->
                viewDefault model

            Post (Err errStr) ->
                appStatusMessage EH.softRed <|
                    "Error interpreting a post url: "
                        ++ errStr

            Post (Ok postIdInfo) ->
                viewPost postIdInfo model

            Topic topic ->
                viewMessagesForTopic topic model
        , if model.showComposeUX then
            Element.el
                [ Element.width Element.fill
                , Element.alignBottom
                ]
                (viewComposeUX
                    (Wallet.userInfo model.wallet
                        |> Maybe.map
                            (\userInfo ->
                                ( userInfo
                                , model.showAddress == Just User
                                )
                            )
                    )
                    model.composeUXModel
                    (case model.viewFilter of
                        Topic topic ->
                            Just topic

                        _ ->
                            Nothing
                    )
                )

          else
            Element.none
        ]


appStatusMessage : Element.Color -> String -> Element Msg
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


header : ViewFilter -> Element Msg
header viewFilter =
    Element.column
        [ Element.Font.bold
        , Element.padding 10
        , Element.spacing 10
        ]
        [ Element.el
            [ Element.pointer
            , Element.Font.size 50
            , Element.Events.onClick <|
                GotoRoute <|
                    Routing.Default
            ]
            (Element.text "SmokeSignal")
        , Element.el
            [ Element.Font.size 40
            , Element.padding 10
            ]
          <|
            case viewFilter of
                None ->
                    Element.none

                Post postIdInfoResult ->
                    Element.text <|
                        "View Post "
                            ++ (postIdInfoResult
                                    |> Result.map
                                        (.messageHash
                                            >> shortenedMessageHash
                                        )
                                    |> Result.withDefault ""
                               )

                Topic topic ->
                    Element.text <| "View Topic " ++ topic
        ]


shortenedMessageHash : Hex -> String
shortenedMessageHash hash =
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


viewDefault : Model -> Element Msg
viewDefault model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Element.inFront <|
            if model.showComposeUX then
                Element.none

            else
                viewMinimizedComposeUX
                    (Wallet.userInfo model.wallet
                        |> Maybe.map
                            (\userInfo ->
                                ( userInfo
                                , model.showAddress == Just User
                                )
                            )
                    )
        ]
        [ Element.Lazy.lazy5
            viewMessagesAndDrafts
            model.blockTimes
            model.messages
            model.replies
            model.miningMessages
            model.showAddress
        ]


viewPost : PostId -> Model -> Element Msg
viewPost postId model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Element.padding 20
        ]
    <|
        (getPostFromId model postId
            |> Maybe.map
                (viewEntireMessageAndReplies model)
            |> Maybe.withDefault
                (appStatusMessage EH.darkGray
                    "Loading post..."
                )
        )


viewMessagesForTopic : String -> Model -> Element Msg
viewMessagesForTopic topic model =
    let
        filteredMessages =
            model.messages
                |> filterBlockMessages
                    (\message ->
                        Message.getTopic message == Just topic
                    )
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Element.padding 20
        , Element.inFront <|
            if model.showComposeUX then
                Element.none

            else
                viewMinimizedComposeUX
                    (Wallet.userInfo model.wallet
                        |> Maybe.map
                            (\userInfo ->
                                ( userInfo
                                , model.showAddress == Just User
                                )
                            )
                    )
        ]
    <|
        if Dict.isEmpty filteredMessages then
            appStatusMessage EH.darkGray <| "No messages found with topic '" ++ topic ++ "'"

        else
            viewMinedMessages
                model.blockTimes
                filteredMessages
                model.replies
                model.showAddress


viewMinimizedComposeUX : Maybe ( UserInfo, Bool ) -> Element Msg
viewMinimizedComposeUX maybeUserInfoAndShowAddress =
    let
        commonAttributes =
            [ Element.alignLeft
            , Element.alignBottom
            , Element.Background.color EH.lightBlue
            , Element.padding 10
            , Element.Border.roundEach
                { topLeft = 0
                , topRight = 10
                , bottomRight = 0
                , bottomLeft = 0
                }
            , composeUXShadow
            , Element.Border.color (Element.rgba 0 0 1 0.5)
            , Element.Border.widthEach
                { top = 1
                , right = 1
                , bottom = 0
                , left = 0
                }
            ]
    in
    case maybeUserInfoAndShowAddress of
        Nothing ->
            Element.el commonAttributes <|
                web3ConnectButton []

        Just ( accountInfo, showAddress ) ->
            Element.column
                (commonAttributes
                    ++ [ Element.spacing 3
                       ]
                )
            <|
                [ phaceElement
                    User
                    accountInfo.address
                    showAddress
                , EH.blueButton
                    Mobile
                    [ Element.width Element.fill ]
                    [ "Compose" ]
                    (ShowComposeUX True)
                ]


composeUXShadow : Attribute Msg
composeUXShadow =
    Element.Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = EH.darkGray
        }


viewMessagesAndDrafts : Dict Int Time.Posix -> Dict Int (List Message) -> List Reply -> Dict String MiningMessage -> Maybe PhaceId -> Element Msg
viewMessagesAndDrafts blockTimes messages replies miningMessages maybeShowAddressForPhace =
    if Dict.isEmpty messages then
        appStatusMessage EH.darkGray
            "Searching for SmokeSignal messages..."

    else
        Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            ]
        <|
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 20
                , Element.paddingEach
                    { bottom = 140
                    , top = 20
                    , right = 20
                    , left = 20
                    }
                ]
                [ viewMiningMessages miningMessages maybeShowAddressForPhace
                , viewMinedMessages blockTimes messages replies maybeShowAddressForPhace
                ]


viewMiningMessages : Dict String MiningMessage -> Maybe PhaceId -> Element Msg
viewMiningMessages miningMessages showAddress =
    let
        miningMessagesList =
            if (List.length <| Dict.toList miningMessages) == 0 then
                []

            else
                [ Element.el
                    [ Element.Font.size 26 ]
                    (Element.text "Your mining messages")
                , Element.column
                    [ Element.paddingXY 20 0
                    , Element.spacing 20
                    ]
                    (Dict.map
                        (viewMiningMessage
                            (case showAddress of
                                Just (PhaceForUserMiningMessage txHash) ->
                                    Just txHash

                                _ ->
                                    Nothing
                            )
                        )
                        miningMessages
                        |> Dict.values
                    )
                ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        miningMessagesList


viewMinedMessages : Dict Int Time.Posix -> Dict Int (List Message) -> List Reply -> Maybe PhaceId -> Element Msg
viewMinedMessages blockTimes messages replies showAddress =
    let
        messagesList =
            messages
                |> Dict.map
                    (\blocknum messagesForBlock ->
                        Element.column
                            [ Element.paddingXY 20 0
                            , Element.spacing 20
                            ]
                            (List.map
                                (\message ->
                                    viewEntireMessage
                                        (case showAddress of
                                            Just (PhaceForMinedMessage postId) ->
                                                Just postId

                                            _ ->
                                                Nothing
                                        )
                                        (replies
                                            |> List.Extra.count
                                                (.to >> (==) message.postId)
                                            |> (\n ->
                                                    if n > 0 then
                                                        Just n

                                                    else
                                                        Nothing
                                               )
                                        )
                                        message
                                )
                                messagesForBlock
                            )
                    )
                |> Dict.toList
                |> List.reverse
                |> List.map
                    (\( blocknum, messagesEl ) ->
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing 10
                            ]
                            [ Element.column
                                [ Element.width Element.fill
                                , Element.spacing 5
                                , Element.Font.italic
                                , Element.Font.size 14
                                ]
                                [ Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 5
                                    ]
                                    [ Element.text <| "block " ++ String.fromInt blocknum
                                    , Element.el
                                        [ Element.width Element.fill
                                        , Element.height <| Element.px 1
                                        , Element.Border.color EH.black
                                        , Element.Border.widthEach
                                            { top = 1
                                            , bottom = 0
                                            , right = 0
                                            , left = 0
                                            }
                                        , Element.Border.dashed
                                        ]
                                        Element.none
                                    ]
                                , blockTimes
                                    |> Dict.get blocknum
                                    |> Maybe.map posixToString
                                    |> Maybe.withDefault "[fetching block timestamp]"
                                    |> Element.text
                                ]
                            , messagesEl
                            ]
                    )
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        messagesList


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


viewEntireMessageAndReplies : Model -> Message -> Element Msg
viewEntireMessageAndReplies model message =
    let
        replyingMessages =
            let
                messageIds =
                    model.replies
                        |> List.filterMap
                            (\reply ->
                                if reply.to == message.postId then
                                    Just reply.from

                                else
                                    Nothing
                            )
            in
            messageIds
                |> List.map (getPostFromId model)
                |> Maybe.Extra.values
                |> Dict.Extra.groupBy (.postId >> .block)
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 40
        ]
        [ viewEntireMessage
            (case model.showAddress of
                Just (PhaceForMinedMessage messageIdInfo) ->
                    Just messageIdInfo

                _ ->
                    Nothing
            )
            Nothing
            message
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 20
            , Element.paddingEach
                { left = 40
                , right = 0
                , top = 0
                , bottom = 0
                }
            ]
            [ Element.el
                [ Element.Font.size 40
                , Element.Font.bold
                ]
                (Element.text "Replies")
            , viewMinedMessages
                model.blockTimes
                replyingMessages
                model.replies
                model.showAddress
            ]
        ]


viewEntireMessage : Maybe PostId -> Maybe Int -> Message -> Element Msg
viewEntireMessage maybeShowAddressPostId maybeNumReplies message =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.el
            [ Element.alignTop
            , Element.height <| Element.px 100
            ]
          <|
            phaceElement
                (PhaceForMinedMessage message.postId)
                message.from
                (maybeShowAddressPostId == Just message.postId)
        , Element.column
            [ Element.width Element.fill
            , Element.alignTop
            ]
            [ Element.row
                [ Element.width Element.fill ]
                [ viewDaiBurned message.burnAmount
                , viewPermalink message.postId
                ]
            , viewMainMessageBlock (MinedMessage message)
            , Maybe.map
                (viewNumReplies message.postId)
                maybeNumReplies
                |> Maybe.withDefault Element.none
            ]
        ]


viewNumReplies : PostId -> Int -> Element Msg
viewNumReplies postId numReplies =
    Element.el
        [ Element.Font.color EH.blue
        , Element.pointer
        , Element.Events.onClick <|
            GotoRoute <|
                Routing.ViewPost <|
                    Ok postId
        , Element.Font.italic
        , Element.paddingXY 20 10
        ]
        (Element.text <|
            String.fromInt numReplies
                ++ (if numReplies == 1 then
                        " reply"

                    else
                        " replies"
                   )
        )


viewMiningMessage : Maybe TxHash -> String -> MiningMessage -> Element Msg
viewMiningMessage maybeShowAddressForMessage txHashIdString miningMessage =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.inFront <|
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color <| Element.rgba 1 1 1 0.6
                ]
                Element.none
        ]
        [ Element.el
            [ Element.alignTop
            , Element.height <| Element.px 100
            ]
          <|
            phaceElement
                (PhaceForUserMiningMessage (Eth.Utils.unsafeToTxHash txHashIdString))
                miningMessage.draft.author
                (maybeShowAddressForMessage == Just (Eth.Utils.unsafeToTxHash txHashIdString))
        , Element.column
            [ Element.width <| Element.px 100
            , Element.alignTop
            , Element.width Element.fill
            ]
            [ Element.row
                [ Element.spacing 5 ]
                [ viewDaiBurned miningMessage.draft.burnAmount
                , viewMiningMessageStatus txHashIdString miningMessage.status
                ]
            , viewMainMessageBlock (MiningMessage miningMessage)
            ]
        ]


viewMiningMessageStatus : String -> MiningMessageStatus -> Element Msg
viewMiningMessageStatus txHashString status =
    Element.column
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.spacing 5
        , Element.Background.color <|
            case status of
                Failed _ ->
                    EH.softRed

                _ ->
                    Element.rgb 1 1 0
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignLeft
        ]
        [ Element.text (statusToString status)
        , Element.row
            [ Element.spacing 8 ]
            [ Element.text "Tx hash:"
            , Element.newTabLink
                [ Element.Font.color EH.blue ]
                { url = EthHelpers.etherscanTxUrl (Eth.Utils.unsafeToTxHash txHashString)
                , label = Element.text txHashString
                }
            ]
        ]


statusToString : MiningMessageStatus -> String
statusToString status =
    case status of
        Mining ->
            "Mining"

        Failed errStr ->
            "Error: " ++ errStr


viewDaiBurned : TokenValue -> Element Msg
viewDaiBurned amount =
    Element.el
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.Background.color EH.lightRed
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignLeft
        ]
    <|
        Element.row
            [ Element.spacing 3
            ]
            [ daiSymbol [ Element.height <| Element.px 18 ]
            , Element.text <| TokenValue.toConciseString amount
            ]


viewPermalink : PostId -> Element Msg
viewPermalink postIdInfo =
    Element.el
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.Background.color EH.lightBlue
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignRight
        ]
    <|
        Element.link
            [ Element.Font.color EH.blue
            , Element.Font.size 16
            ]
            { url =
                Routing.routeToFullDotEthUrlString <|
                    Routing.ViewPost <|
                        Ok postIdInfo
            , label = Element.text ".eth permalink"
            }


type PossiblyMiningMessage
    = MinedMessage Message
    | MiningMessage MiningMessage


getContent : PossiblyMiningMessage -> String
getContent possiblyMiningMessage =
    case possiblyMiningMessage of
        MinedMessage message ->
            message.message

        MiningMessage miningMessage ->
            miningMessage.draft.message


getMetadata : PossiblyMiningMessage -> Result Json.Decode.Error Message.Metadata
getMetadata possiblyMiningMessage =
    case possiblyMiningMessage of
        MinedMessage message ->
            message.metadata

        MiningMessage miningMessage ->
            Ok <| miningMessage.draft.metadata


viewMainMessageBlock : PossiblyMiningMessage -> Element Msg
viewMainMessageBlock possiblyMiningMessage =
    let
        renderResult =
            renderMarkdownParagraphs
                [ Element.spacing 15 ]
                (getContent possiblyMiningMessage)
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 20
        , Element.spacing 20
        , Element.Background.color (Element.rgb 0.8 0.8 1)
        , Element.Border.roundEach
            { topLeft = 0
            , topRight =
                case possiblyMiningMessage of
                    MinedMessage _ ->
                        0

                    _ ->
                        10
            , bottomRight = 10
            , bottomLeft = 10
            }
        , Element.alignTop
        ]
        [ metadataStuff possiblyMiningMessage
        , case renderResult of
            Ok rendered ->
                rendered

            Err errStr ->
                Element.el
                    [ Element.Font.color EH.softRed
                    , Element.Font.italic
                    ]
                <|
                    Element.text <|
                        "Error parsing/rendering markdown:"
                            ++ errStr
        , messageActions possiblyMiningMessage
        ]


metadataStuff : PossiblyMiningMessage -> Element Msg
metadataStuff possiblyMiningMessage =
    case getMetadata possiblyMiningMessage of
        Err jsonDecodeErr ->
            viewMetadataDecodeError jsonDecodeErr

        Ok metadata ->
            Element.row
                [ Element.width Element.fill
                , Element.spacing 20
                ]
                ([ maybeViewReplyInfo metadata.replyTo Nothing
                 , Maybe.map
                    (Element.el [ Element.alignRight ])
                    (maybeViewTopic metadata.topic)
                 ]
                    |> Maybe.Extra.values
                )


viewMetadataDecodeError : Json.Decode.Error -> Element Msg
viewMetadataDecodeError error =
    Element.el
        [ Element.Font.color EH.softRed
        , Element.Font.italic
        , Element.Font.size 18
        ]
        (Element.text <|
            "Message contains malformed metadata: "
                ++ Json.Decode.errorToString error
        )


maybeViewReplyInfo : Maybe PostId -> Maybe Msg -> Maybe (Element Msg)
maybeViewReplyInfo maybePostId maybeCloseMsg =
    case maybePostId of
        Nothing ->
            Nothing

        Just postId ->
            Just <|
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
                            [ Element.Font.color EH.blue
                            , Element.pointer
                            , Element.Events.onClick <|
                                GotoRoute <|
                                    Routing.ViewPost (Ok postId)
                            ]
                            (Element.text <|
                                shortenedMessageHash postId.messageHash
                            )
                        ]
                    , case maybeCloseMsg of
                        Just closeMsg ->
                            Element.image
                                [ Element.padding 4
                                , Element.alignTop
                                , Element.pointer
                                , Element.Events.onClick closeMsg
                                ]
                                { src = "img/remove-circle-black.svg"
                                , description = "remove"
                                }

                        Nothing ->
                            Element.none
                    ]


maybeViewTopic : Maybe String -> Maybe (Element Msg)
maybeViewTopic maybeTopic =
    case maybeTopic of
        Nothing ->
            Nothing

        Just topic ->
            Just <|
                viewTopic topic


viewTopic : String -> Element Msg
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
        [ Element.text "Message topic:"
        , Element.el
            [ Element.Font.color EH.blue
            , Element.pointer
            , Element.Events.onClick <|
                GotoRoute <|
                    Routing.ViewTopic topic
            ]
            (Element.text topic)
        ]


messageActions : PossiblyMiningMessage -> Element Msg
messageActions possiblyMiningMessage =
    case possiblyMiningMessage of
        MinedMessage message ->
            Element.row
                [ Element.alignRight ]
                [ replyButton message.postId ]

        MiningMessage _ ->
            Element.none


replyButton : PostId -> Element Msg
replyButton postId =
    Element.el
        [ Element.padding 7
        , Element.pointer
        , Element.Border.rounded 4
        , Element.Background.color <| Element.rgba 1 1 1 0.3
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 5
            , color = Element.rgba 0 0 0 0.1
            }
        , Element.Events.onClick (UpdateReplyTo <| Just postId)
        , Element.width <| Element.px 30
        ]
    <|
        Element.image
            [ Element.width Element.fill ]
            { src = "img/reply-arrow.svg"
            , description = "reply"
            }


renderMarkdownParagraphs : List (Attribute Msg) -> String -> Result String (Element Msg)
renderMarkdownParagraphs attributes =
    ElementMarkdown.renderString attributes


viewComposeUX : Maybe ( UserInfo, Bool ) -> ComposeUXModel -> Maybe String -> Element Msg
viewComposeUX maybeUserInfoAndShowAddress composeUXModel maybeTopic =
    Element.column
        [ Element.width Element.fill
        , Element.Background.color EH.lightBlue
        , Element.padding 10
        , composeUXShadow
        , Element.Border.roundEach
            { topLeft = 0
            , topRight = 10
            , bottomRight = 10
            , bottomLeft = 10
            }
        , Element.above <|
            Element.el
                [ Element.alignLeft
                ]
            <|
                EH.blueButton
                    Mobile
                    []
                    [ "Hide" ]
                    (ShowComposeUX False)
        ]
        [ viewComposeMetadata composeUXModel.replyTo maybeTopic
        , messageInputBox composeUXModel.message
        , actionFormAndMaybeErrorEl maybeUserInfoAndShowAddress composeUXModel maybeTopic
        ]


viewComposeMetadata : Maybe PostId -> Maybe String -> Element Msg
viewComposeMetadata maybeReplyTo maybeTopic =
    Maybe.map
        (Element.row
            [ Element.spacing 10
            , Element.paddingXY 30 10
            , Element.width Element.fill
            ]
        )
        ([ maybeViewReplyInfo maybeReplyTo (Just <| UpdateReplyTo Nothing)
         , Maybe.map (Element.el [ Element.alignRight ] << viewTopic) maybeTopic
         ]
            |> Maybe.Extra.values
            |> ListHelpers.nonEmpty
        )
        |> Maybe.withDefault Element.none


messageInputBox : String -> Element Msg
messageInputBox input =
    Element.el
        [ Element.width Element.fill
        , Element.padding 10
        , Element.Border.roundEach
            { bottomRight = 0
            , bottomLeft = 10
            , topRight = 10
            , topLeft = 10
            }
        , Element.Background.color EH.lightBlue
        ]
    <|
        Element.Input.multiline
            [ Element.width Element.fill
            , Element.height (Element.px 300)
            , Element.Background.color <| Element.rgba 1 1 1 0.5
            ]
            { onChange = MessageInputChanged
            , text = input
            , placeholder = Just messageInputPlaceholder
            , label = Element.Input.labelHidden "messageInput"
            , spellcheck = True
            }


actionFormAndMaybeErrorEl : Maybe ( UserInfo, Bool ) -> ComposeUXModel -> Maybe String -> Element Msg
actionFormAndMaybeErrorEl maybeUserInfoAndShowAddress composeUXModel maybeTopic =
    case maybeUserInfoAndShowAddress of
        Just ( userInfo, showAddress ) ->
            let
                ( goButtonEl, maybeErrorEls ) =
                    goButtonAndMaybeError userInfo composeUXModel maybeTopic
            in
            Element.row
                [ Element.alignLeft
                , Element.spacing 10
                ]
                [ Element.row
                    [ Element.spacing 15
                    , Element.padding 10
                    , Element.Background.color <| Element.rgb 0.8 0.8 1
                    , Element.Border.rounded 10
                    ]
                    [ phaceElement User userInfo.address showAddress
                    , inputsElement userInfo composeUXModel
                    , goButtonEl
                    ]
                , inputErrorEl maybeErrorEls
                ]

        Nothing ->
            web3ConnectButton [ Element.centerX, Element.centerY ]


inputErrorEl : Maybe (List (Element Msg)) -> Element Msg
inputErrorEl =
    Maybe.map
        (Element.paragraph
            [ Element.width (Element.fill |> Element.maximum 700)
            , Element.Font.color EH.softRed
            , Element.Font.italic
            ]
        )
        >> Maybe.withDefault Element.none


inputsElement : UserInfo -> ComposeUXModel -> Element Msg
inputsElement userInfo composeUXModel =
    Element.el
        [ Element.centerY
        , Element.width <| Element.px 260
        ]
    <|
        Element.el [ Element.centerX ] <|
            case composeUXModel.miningUnlockTx of
                Just txHash ->
                    Element.column
                        [ Element.spacing 4
                        , Element.Font.color (Element.rgb 0.3 0.3 0.3)
                        , Element.centerY
                        ]
                        [ Element.el
                            [ Element.centerX ]
                            (Element.text "Mining DAI unlock tx...")
                        , Element.newTabLink
                            [ Element.Font.color EH.blue
                            , Element.Font.size 14
                            , Element.centerX
                            ]
                            { url = EthHelpers.etherscanTxUrl txHash
                            , label = Element.text "(track on etherscan)"
                            }
                        , Element.el
                            [ Element.centerX ]
                            (Element.text "Feel free to draft your message")
                        , Element.el
                            [ Element.centerX ]
                            (Element.text "while waiting!")
                        ]

                Nothing ->
                    case userInfo.daiUnlocked of
                        Nothing ->
                            loadingElement
                                [ Element.centerX
                                , Element.centerY
                                ]
                            <|
                                Just "Checking DAI lock..."

                        Just False ->
                            unlockButton
                                [ Element.centerX
                                , Element.centerY
                                ]

                        Just True ->
                            Element.column
                                [ Element.spacing 10 ]
                                [ Element.row
                                    [ Element.spacing 10
                                    , Element.centerX
                                    ]
                                    [ Element.text "Burn"
                                    , burnAmountInput composeUXModel.daiInput
                                    , Element.text "DAI"
                                    ]
                                , Element.row
                                    [ Element.Font.size 14
                                    , Element.spacing 5
                                    ]
                                    [ Element.Input.checkbox [ Element.alignTop ]
                                        { onChange = DonationCheckboxSet
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = composeUXModel.donateChecked
                                        , label = Element.Input.labelHidden "Donate an extra 1% to Foundry"
                                        }
                                    , Element.column
                                        [ Element.spacing 5 ]
                                        [ Element.row []
                                            [ Element.text "Donate an extra 1% to "
                                            , Element.newTabLink
                                                [ Element.Font.color EH.blue ]
                                                { url = "https://foundrydao.com/"
                                                , label = Element.text "Foundry"
                                                }
                                            ]
                                        , Element.text "so we can build more cool stuff!"
                                        ]
                                    ]
                                ]


goButtonAndMaybeError : UserInfo -> ComposeUXModel -> Maybe String -> ( Element Msg, Maybe (List (Element Msg)) )
goButtonAndMaybeError userInfo composeUXModel maybeTopic =
    case userInfo.balance of
        Just balance ->
            if TokenValue.isZero balance then
                ( maybeGoButton Nothing
                , Just
                    [ Element.text <|
                        "That account ("
                            ++ Eth.Utils.addressToChecksumString userInfo.address
                            ++ ") doesn't have any DAI! "
                    , Element.newTabLink [ Element.Font.color EH.blue ]
                        { url = "https://kyberswap.com/swap/eth-dai"
                        , label = Element.text "Kyberswap"
                        }
                    , Element.text " can swap your ETH for DAI in a single transaction."
                    ]
                )

            else
                case userInfo.daiUnlocked of
                    Just True ->
                        let
                            validateResults =
                                validateInputs composeUXModel
                        in
                        case validateResults.burnAndDonateAmount of
                            Just (Ok ( burnAmount, donateAmount )) ->
                                let
                                    balanceTooLow =
                                        TokenValue.compare
                                            (TokenValue.add burnAmount donateAmount)
                                            balance
                                            == GT
                                in
                                if balanceTooLow then
                                    ( maybeGoButton Nothing
                                    , Just
                                        [ Element.text "You don't have that much DAI in your wallet! "
                                        , Element.newTabLink [ Element.Font.color EH.blue ]
                                            { url = "https://kyberswap.com/swap/eth-dai"
                                            , label = Element.text "Kyberswap"
                                            }
                                        , Element.text " can swap your ETH for DAI in a single transaction."
                                        ]
                                    )

                                else
                                    case validateResults.message of
                                        Just message ->
                                            ( maybeGoButton <|
                                                Just <|
                                                    Message.Draft
                                                        userInfo.address
                                                        message
                                                        burnAmount
                                                        donateAmount
                                                        (Message.versionedMetadata
                                                            validateResults.replyTo
                                                            maybeTopic
                                                        )
                                            , Nothing
                                            )

                                        Nothing ->
                                            ( maybeGoButton Nothing
                                            , Nothing
                                            )

                            Just (Err errStr) ->
                                ( maybeGoButton Nothing
                                , Just [ Element.text errStr ]
                                )

                            Nothing ->
                                ( maybeGoButton Nothing
                                , Nothing
                                )

                    _ ->
                        ( maybeGoButton Nothing
                        , Nothing
                        )

        _ ->
            ( maybeGoButton Nothing
            , Nothing
            )


maybeGoButton : Maybe Message.Draft -> Element Msg
maybeGoButton maybeDraft =
    let
        commonStyles =
            [ Element.height <| Element.px 100
            , Element.width <| Element.px 100
            , Element.Font.size 26
            , Element.Border.rounded 10
            ]
    in
    case maybeDraft of
        Just draft ->
            EH.redButton
                Desktop
                commonStyles
                [ "Post" ]
                (Submit draft)

        Nothing ->
            EH.disabledButton
                Desktop
                commonStyles
                "Post"
                Nothing


loadingElement : List (Attribute Msg) -> Maybe String -> Element Msg
loadingElement attrs maybeString =
    Element.el
        ([ Element.Font.italic
         , Element.Font.color EH.darkGray
         , Element.Font.size 20
         ]
            ++ attrs
        )
        (Element.text <| Maybe.withDefault "loading..." maybeString)


web3ConnectButton : List (Attribute Msg) -> Element Msg
web3ConnectButton attrs =
    EH.redButton
        Mobile
        attrs
        [ "Connect to Wallet" ]
        ConnectToWeb3


unlockButton : List (Attribute Msg) -> Element Msg
unlockButton attrs =
    EH.redButton
        Desktop
        attrs
        [ "Unlock Dai" ]
        UnlockDai


burnAmountInput : String -> Element Msg
burnAmountInput daiInput =
    Element.row []
        [ Element.Input.text
            [ Element.width <| Element.px 100
            , Element.Background.color <| Element.rgba 1 1 1 0.4
            ]
            { onChange = DaiInputChanged
            , text = daiInput
            , placeholder = Nothing
            , label = Element.Input.labelHidden "amount to burn"
            }
        ]


showComposeUXButton : Element Msg
showComposeUXButton =
    EH.blueButton
        Desktop
        []
        [ "Compose Message" ]
        (ShowComposeUX True)


messageInputPlaceholder : Element.Input.Placeholder Msg
messageInputPlaceholder =
    Element.Input.placeholder [] <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
        <|
            List.map
                (Element.paragraph
                    [ Element.Font.color EH.darkGray
                    , Element.Font.italic
                    ]
                    << List.map Element.text
                )
                [ [ "SmokeSignal messages are formatted with markdown (e.g. *italic*, **bold**, [link-title](url))." ]
                , [ "Hackmd.io is useful for drafting and previewing markdown text." ]
                ]


phaceElement : PhaceId -> Address -> Bool -> Element Msg
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


userNoticeEls : DisplayProfile -> List (UserNotice Msg) -> List (Element Msg)
userNoticeEls dProfile notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft (20 |> changeForMobile 5 dProfile)
            , Element.moveUp (20 |> changeForMobile 5 dProfile)
            , Element.spacing (10 |> changeForMobile 5 dProfile)
            , Element.alignRight
            , Element.alignBottom
            , Element.width <| Element.px (300 |> changeForMobile 150 dProfile)
            , Element.Font.size (15 |> changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map (userNotice dProfile)
            )
        , Element.column
            [ Element.moveRight (20 |> changeForMobile 5 dProfile)
            , Element.moveDown 100
            , Element.spacing (10 |> changeForMobile 5 dProfile)
            , Element.alignLeft
            , Element.alignTop
            , Element.width <| Element.px (300 |> changeForMobile 150 dProfile)
            , Element.Font.size (15 |> changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map (userNotice dProfile)
            )
        ]


userNotice : DisplayProfile -> ( Int, UserNotice Msg ) -> Element Msg
userNotice dProfile ( id, notice ) =
    let
        color =
            case notice.noticeType of
                UN.Update ->
                    Element.rgb255 100 200 255

                UN.Caution ->
                    Element.rgb255 255 188 0

                UN.Error ->
                    Element.rgb255 255 70 70

                UN.ShouldBeImpossible ->
                    Element.rgb255 200 200 200

        textColor =
            case notice.noticeType of
                UN.Error ->
                    Element.rgb 1 1 1

                _ ->
                    Element.rgb 0 0 0

        closeElement =
            Element.el
                [ Element.alignRight
                , Element.alignTop
                , Element.moveUp 5
                , Element.moveRight 5
                ]
                (EH.closeButton True (DismissNotice id))
    in
    Element.el
        [ Element.Background.color color
        , Element.Border.rounded (10 |> changeForMobile 5 dProfile)
        , Element.padding (8 |> changeForMobile 3 dProfile)
        , Element.width Element.fill
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.15
        , EH.subtleShadow
        , EH.onClickNoPropagation NoOp
        ]
        (notice.mainParagraphs
            |> List.indexedMap
                (\pNum paragraphLines ->
                    Element.paragraph
                        [ Element.width Element.fill
                        , Element.Font.color textColor
                        , Element.spacing 1
                        ]
                        (if pNum == 0 then
                            closeElement :: paragraphLines

                         else
                            paragraphLines
                        )
                )
            |> Element.column
                [ Element.spacing 4
                , Element.width Element.fill
                ]
        )


daiSymbol : List (Attribute Msg) -> Element Msg
daiSymbol attributes =
    Element.image attributes
        { src = "img/dai-unit-char.svg"
        , description = ""
        }
