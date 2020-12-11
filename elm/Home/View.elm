module Home.View exposing (view)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Embed.Youtube
import Embed.Youtube.Attributes
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Tuple as TupleHelpers
import Home.Types exposing (..)
import Html.Attributes exposing (list)
import Post exposing (Post)
import PostUX.Preview as PostPreview
import PostUX.Types as PostUX
import Routing exposing (Route)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view :
    EH.DisplayProfile
    -> Maybe PhaceIconId
    -> WalletUXPhaceInfo
    -> Bool
    -> PublishedPostsDict
    -> Element Msg
view dProfile showAddressId walletUXPhaceInfo donateChecked posts =
    let
        listOfPosts =
            Dict.values posts

        maybeShowAddressForId =
            case showAddressId of
                Just (PhaceForPublishedPost id) ->
                    Just id

                _ ->
                    Nothing
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color darkTheme.appBackground
        , responsiveVal dProfile
            (Element.paddingXY 40 40)
            (Element.paddingXY 10 20)
        , Element.Font.color darkTheme.emphasizedTextColor
        ]
    <|
        Element.column
            [ Element.width (Element.fill |> Element.maximum 1100)
            , Element.centerX
            , Element.spacing (responsiveVal dProfile 50 30)
            ]
        <|
            case dProfile of
                Desktop ->
                    [ boldProclamationEl dProfile
                    , tutorialVideo dProfile
                    , conversationAlreadyStartedEl dProfile
                    , Element.row
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing 40
                        ]
                        [ Element.column
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            ]
                          <|
                            List.map
                                (postFeed
                                    dProfile
                                    donateChecked
                                    maybeShowAddressForId
                                )
                                listOfPosts
                        ]
                    ]

                Mobile ->
                    [ boldProclamationEl dProfile
                    , tutorialVideo dProfile
                    , conversationAlreadyStartedEl dProfile
                    , case walletUXPhaceInfo of
                        DemoPhaceInfo _ ->
                            web3ConnectButton
                                dProfile
                                [ Element.width Element.fill ]
                                MsgUp

                        _ ->
                            Element.column
                                [ Element.width Element.fill
                                , Element.spacing 10
                                ]
                                [ defaultTheme.greenActionButton
                                    dProfile
                                    [ Element.width Element.fill ]
                                    [ "Create a New Post" ]
                                    (MsgUp <|
                                        GotoRoute <|
                                            Routing.Compose <|
                                                Post.TopLevel Post.defaultTopic
                                    )
                                ]

                    --, infoBlock dProfile
                    --, conversationAlreadyStartedEl dProfile
                    -- , topicsBlock dProfile posts
                    -- , topicsExplainerEl dProfile
                    --, composeActionBlock dProfile walletUXPhaceInfo
                    ]


postFeed :
    DisplayProfile
    -> Bool
    -> Maybe Post.Id
    -> List Post.Published
    -> Element Msg
postFeed dProfile donateChecked maybeShowAddressForId listOfPosts =
    let
        posts =
            List.sortBy (\post -> toFloat post.id.block * TokenValue.toFloatWithWarning post.core.authorBurn / pi)
                listOfPosts
                |> List.reverse
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacingXY 0 5
        , Element.paddingXY 0 20
        ]
    <|
        List.map
            (previewPost
                dProfile
                donateChecked
                maybeShowAddressForId
                Nothing
            )
            posts


previewPost :
    DisplayProfile
    -> Bool
    -> Maybe Post.Id
    -> Maybe PostUX.Model
    -> Post.Published
    -> Element Msg
previewPost dProfile donateChecked maybeShowAddressForId maybePostUXModel post =
    Element.map PostUXMsg <|
        PostPreview.view
            dProfile
            donateChecked
            (maybeShowAddressForId == Just post.id)
            maybePostUXModel
            post


feedEl :
    DisplayProfile
    -> Post.Published
    -> Element Msg
feedEl dProfile post =
    let
        message =
            post.core.content.body

        topic =
            case post.core.metadata.context of
                Post.Reply id ->
                    ""

                Post.TopLevel theTopic ->
                    theTopic
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacingXY 0 5
        ]
        [ Element.text <|
            (topic
                ++ "   (rank: "
                ++ String.fromFloat (toFloat post.id.block * TokenValue.toFloatWithWarning post.core.authorBurn / pi)
                ++ ") "
            )
        , Element.paragraph [ Element.width Element.fill ] <|
            [ Element.text <|
                String.left (responsiveVal dProfile 200 50) message
                    ++ (if String.length message > 50 then
                            "..."

                        else
                            ""
                       )
            ]
        ]


boldProclamationEl : DisplayProfile -> Element Msg
boldProclamationEl dProfile =
    Element.column
        [ Element.centerX
        , Element.Font.bold
        , Element.spacing (responsiveVal dProfile 20 10)
        ]
        [ coloredAppTitle
            [ Element.Font.size (responsiveVal dProfile 80 50)
            , Element.centerX
            ]
        , Element.el
            [ Element.width Element.fill
            , Element.paddingXY
                (responsiveVal dProfile 40 15)
                0
            ]
          <|
            EH.thinHRuler <|
                Element.rgb 1 0 0
        , Element.el
            [ Element.Font.size (responsiveVal dProfile 50 15)
            , Element.centerX
            , Element.Font.color Theme.almostWhite
            ]
          <|
            Element.text "Uncensorable - Immutable - Unkillable"
        , Element.el
            [ Element.Font.size (responsiveVal dProfile 45 15)
            , Element.centerX
            , Element.Font.color Theme.almostWhite
            ]
          <|
            Element.text "Real Free Speech - Cemented on the Blockchain"
        ]


tutorialVideo : DisplayProfile -> Element Msg
tutorialVideo dProfile =
    let
        ( width, height ) =
            responsiveVal dProfile
                ( 854, 480 )
                ( 426, 240 )

        html =
            Embed.Youtube.fromString "pV70Q0wgnnU"
                |> Embed.Youtube.attributes
                    [ Embed.Youtube.Attributes.width width
                    , Embed.Youtube.Attributes.height height
                    ]
                |> Embed.Youtube.toHtml
    in
    Element.el
        [ Element.centerX
        , Element.paddingEach
            { top = 0
            , bottom = 0
            , right = 0
            , left = 0
            }
        ]
    <|
        Element.html html



-- boldProclamationEl : DisplayProfile -> Element Msg
-- boldProclamationEl dProfile =
--     Element.column
--         [ Element.centerX
--         , Element.Font.bold
--         , Element.spacing (responsiveVal dProfile 20 10)
--         ]
--         [ coloredAppTitle
--             [ Element.Font.size (responsiveVal dProfile 80 60)
--             , Element.centerX
--             ]
--         , Element.el
--             [ Element.width Element.fill
--             , Element.paddingXY
--                 (responsiveVal dProfile 40 15)
--                 0
--             ]
--           <|
--             EH.thinHRuler <|
--                 Element.rgb 1 0 0
--         , Element.el
--             [ Element.Font.size (responsiveVal dProfile 50 30)
--             , Element.centerX
--             , Element.Font.color Theme.almostWhite
--             ]
--           <|
--             Element.text "A Bunker for Free Speech"
--         ]


infoBlock : DisplayProfile -> Element Msg
infoBlock dProfile =
    Element.column
        [ Element.Border.rounded 15
        , Element.Background.color Theme.darkBlue
        , Element.padding (responsiveVal dProfile 25 15)
        , Element.Font.color <| EH.white
        , Element.Font.size (responsiveVal dProfile 22 18)
        , Element.Font.color darkTheme.mainTextColor
        , Element.centerX
        , Element.spacing 20
        , Element.width Element.fill
        , Element.alignTop
        ]
    <|
        List.map
            (Element.paragraph
                [ Element.width Element.fill
                , Element.Font.center
                ]
            )
            [ [ Element.text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat." ]
            , [ Element.column
                    [ Element.spacing 3 ]
                    [ Element.el [ Element.centerX ] <| emphasizedText "No usernames."
                    , Element.el [ Element.centerX ] <| emphasizedText "No moderators."
                    , Element.el [ Element.centerX ] <| emphasizedText "No censorship."
                    , Element.el [ Element.centerX ] <| emphasizedText "No deplatforming."
                    ]
              ]
            , [ Element.text "All you need is ETH for gas and DAI to burn." ]
            , [ Element.text "All SmokeSignal posts are permanent and impossible to delete, and can be accessed with any browser via an IPFS Gateway ("
              , Element.newTabLink
                    [ Element.Font.color defaultTheme.linkTextColor ]
                    { url = "https://gateway.ipfs.io/ipfs/QmeXhVyRJYhtpRcQr4uYsJZi6wBYqyEwdjPRjp3EFCtLHQ/#/context/re?block=9956062&hash=0x0a7e09be33cd207ad208f057e26fba8f8343cfd6c536904c20dbbdf87aa2b257"
                    , label = Element.text "example"
                    }
              , Element.text ") or the smokesignal.eth.link mirror ("
              , Element.newTabLink
                    [ Element.Font.color defaultTheme.linkTextColor ]
                    { url = "https://smokesignal.eth.link/#/context/re?block=9956062&hash=0x0a7e09be33cd207ad208f057e26fba8f8343cfd6c536904c20dbbdf87aa2b257"
                    , label = Element.text "example"
                    }
              , Element.text ")."
              ]
            , [ Element.text "If the above two methods prove unreliable, some browsers also support direct smokesignal.eth links ("
              , Element.newTabLink
                    [ Element.Font.color defaultTheme.linkTextColor ]
                    { url = "https://smokesignal.eth/#/context/re?block=9956062&hash=0x0a7e09be33cd207ad208f057e26fba8f8343cfd6c536904c20dbbdf87aa2b257"
                    , label = Element.text "example"
                    }
              , Element.text ") or direct IPFS links ("
              , Element.newTabLink
                    [ Element.Font.color defaultTheme.linkTextColor ]
                    { url = "ipfs://QmeXhVyRJYhtpRcQr4uYsJZi6wBYqyEwdjPRjp3EFCtLHQ/#/context/re?block=9956062&hash=0x0a7e09be33cd207ad208f057e26fba8f8343cfd6c536904c20dbbdf87aa2b257"
                    , label = Element.text "example"
                    }
              , Element.text ")."
              ]
            ]


conversationAlreadyStartedEl : DisplayProfile -> Element Msg
conversationAlreadyStartedEl dProfile =
    Element.paragraph
        [ Element.Font.size (responsiveVal dProfile 50 36)
        , Element.Font.center
        ]
        [ Element.text "The conversation has already started." ]


topicsExplainerEl : DisplayProfile -> Element Msg
topicsExplainerEl dProfile =
    Element.column
        [ Element.Border.rounded 15
        , Element.Background.color <| Element.rgb 0.3 0 0
        , Element.padding (responsiveVal dProfile 25 15)
        , Element.Font.color <| EH.white
        , Element.Font.size (responsiveVal dProfile 22 18)
        , Element.Font.color darkTheme.mainTextColor
        , Element.centerX
        , Element.width Element.fill
        , Element.spacing 20
        ]
    <|
        List.map
            (Element.paragraph
                [ Element.width Element.fill
                , Element.Font.center
                ]
            )
            [ [ Element.text "Users burn DAI to post messages under any given "
              , emphasizedText "topic"
              , Element.text <|
                    ". Theses topics are listed "
                        ++ responsiveVal dProfile "here" "above"
                        ++ ", along with the "
              , emphasizedText "total DAI burned"
              , Element.text " in that topic."
              ]
            , [ Element.text "If you have a web3 wallet, ETH, and DAI, starting a new topic is easy: type it into the search input, and click "
              , emphasizedText "Start new topic."
              ]
            , [ Element.text " You can then compose the first post for your brand new topic!"
              ]
            ]


composeActionBlock : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
composeActionBlock dProfile walletUXPhaceInfo =
    let
        paragrapher paras =
            Element.column
                [ Element.spacing 15 ]
                (List.map
                    (Element.paragraph
                        [ Element.Font.size (responsiveVal dProfile 22 18)
                        , Element.width Element.fill
                        , Element.Font.color darkTheme.mainTextColor
                        ]
                    )
                    paras
                )
    in
    Element.column
        [ Element.spacing 25
        , Element.centerX
        , Element.width <| Element.px 500
        ]
        [ Element.row
            [ Element.spacing 40
            , Element.centerX
            ]
            [ homeWalletUX dProfile walletUXPhaceInfo
            , Element.column
                [ Element.spacing 5
                , Element.Font.size (responsiveVal dProfile 40 30)
                , Element.Font.bold
                , Element.alignBottom
                ]
                (case walletUXPhaceInfo of
                    UserPhaceInfo _ ->
                        [ Element.text "That's your Phace!"
                        , Element.text "What a cutie."
                        ]

                    DemoPhaceInfo _ ->
                        [ Element.text "Don your Phace."
                        , Element.text "Have your say."
                        ]
                )
            ]
        , paragrapher <|
            case walletUXPhaceInfo of
                UserPhaceInfo _ ->
                    [ [ Element.text "If you don't like that Phace, try switching accounts in your wallet." ]
                    , [ Element.text "Otherwise, you're now free to cavort all over SmokeSignal and wreak all sorts of "
                      , emphasizedText "immutable havoc."
                      , Element.text " Browse the topics above or create your own, or click below to read more about what SmokeSignal can be used for."
                      ]
                    ]

                DemoPhaceInfo _ ->
                    [ [ Element.text "Your Ethereum address maps to a unique Phace, which will be shown next to any SmokeSignal posts you write." ]
                    , [ Element.text "Connect your Web3 Wallet to see your Phace." ]
                    ]
        , case walletUXPhaceInfo of
            DemoPhaceInfo _ ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ web3ConnectButton
                        dProfile
                        [ Element.width Element.fill ]
                        MsgUp
                    , moreInfoButton dProfile
                    ]

            _ ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ moreInfoButton dProfile
                    , defaultTheme.greenActionButton
                        dProfile
                        [ Element.width Element.fill ]
                        [ "Create a New Post" ]
                        (MsgUp <|
                            GotoRoute <|
                                Routing.Compose <|
                                    Post.TopLevel "noob-ramblings-plz-ignore"
                        )
                    ]
        ]


moreInfoButton : DisplayProfile -> Element Msg
moreInfoButton dProfile =
    defaultTheme.secondaryActionButton
        dProfile
        [ Element.width Element.fill ]
        [ "What Can SmokeSignal be Used For?" ]
        (MsgUp <|
            GotoRoute <|
                Routing.ViewContext <|
                    Post.Reply <|
                        Config.moreInfoPostId
        )


homeWalletUX : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
homeWalletUX dProfile walletUXPhaceInfo =
    Element.map MsgUp <|
        case walletUXPhaceInfo of
            DemoPhaceInfo demoAddress ->
                Element.el
                    [ Element.pointer
                    , Element.Events.onClick <| ConnectToWeb3
                    , Element.Border.rounded 10
                    , Element.Border.glow
                        (Element.rgba 1 0 1 0.3)
                        9
                    ]
                <|
                    phaceElement
                        True
                        (Eth.Utils.unsafeToAddress demoAddress)
                        False
                        (ShowOrHideAddress MorphingPhace)
                        NoOp

            UserPhaceInfo ( accountInfo, showAddress ) ->
                Element.el
                    [ Element.Border.rounded 10
                    , Element.Border.glow
                        (Element.rgba 0 0.5 1 0.4)
                        9
                    ]
                <|
                    phaceElement
                        True
                        accountInfo.address
                        showAddress
                        (ShowOrHideAddress UserPhace)
                        NoOp
