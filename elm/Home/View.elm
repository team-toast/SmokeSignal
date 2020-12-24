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
import Helpers.Time as TimeHelpers
import Helpers.Tuple as TupleHelpers
import Home.Types exposing (..)
import Html.Attributes exposing (list)
import Post exposing (Post)
import PostUX.Preview as PostPreview
import PostUX.Types as PostUX
import Routing exposing (Route)
import Theme exposing (theme)
import Time
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view :
    EH.DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe PhaceIconId
    -> String
    -> Wallet
    -> PublishedPostsDict
    -> Model
    -> Element Msg
view dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model =
    case dProfile of
        Desktop ->
            Element.column
                [ Element.centerX
                , Element.width <| Element.px 1000
                , Element.spacing majorSpacing
                ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.paddingXY 10 0
                    ]
                    banner
                , body dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model
                ]

        Mobile ->
            Debug.todo ""


banner : Element Msg
banner =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 200
        , Element.Background.color EH.black
        , whiteGlowAttribute
        ]
    <|
        Element.el
            [ Element.centerX
            ]
            (Element.text "hi")


body :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe PhaceIconId
    -> String
    -> Wallet
    -> PublishedPostsDict
    -> Model
    -> Element Msg
body dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model =
    Element.row
        [ Element.width Element.fill
        , Element.spacing majorSpacing
        ]
        [ Element.column
            [ Element.spacing majorSpacing
            , Element.alignTop
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            , Element.clip
            , Element.width <| Element.fillPortion 2
            , Element.padding 10
            ]
            [ newToSmokeSignalEl dProfile
            , Element.column
                [ Element.spacing 15
                , Element.width Element.fill
                , Element.clipX
                ]
                [ topicsUX dProfile model.topicSearchInput
                , let
                    maybeShowAddressForPostId =
                        case showAddressId of
                            Just (PhaceForPublishedPost id) ->
                                Just id

                            _ ->
                                Nothing
                  in
                  mainPostFeed dProfile donateChecked blockTimes now maybeShowAddressForPostId posts
                ]
            ]
        , Element.column
            [ Element.width <| Element.fillPortion 1
            ]
            [
            ]
        ]


newToSmokeSignalEl :
    DisplayProfile
    -> Element Msg
newToSmokeSignalEl dProfile =
    Element.el
        [ Element.width Element.fill
        , Element.padding 20
        , Element.Font.size 40
        , Element.Background.color Theme.orange
        , Element.Font.semiBold
        , Element.Font.color EH.white
        , whiteGlowAttribute
        , Element.pointer
        ]
    <|
        Element.text "NEW TO SMOKESIGNAL?"


topicsUX :
    DisplayProfile
    -> String
    -> Element Msg
topicsUX dProfile topicsSearchInput =
    Element.none


mainPostFeed :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Post.Id
    -> PublishedPostsDict
    -> Element Msg
mainPostFeed dProfile donateChecked blockTimes now maybeShowAddressForPostId posts =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 40
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clip
            ]
          <|
            [ postFeed
                dProfile
                donateChecked
                blockTimes
                now
                maybeShowAddressForPostId
                (Dict.values posts |> List.concat)
            ]
        ]


-- Element.el
--     [ Element.width Element.fill
--     , Element.height Element.fill
--     , responsiveVal dProfile
--         (Element.paddingXY 40 40)
--         (Element.paddingXY 10 20)
--     , Element.Font.color theme.emphasizedTextColor
--     ]
-- <|
--     Element.column
--         [ Element.width (Element.fill |> Element.maximum 1100)
--         , Element.centerX
--         , Element.spacing (responsiveVal dProfile 50 30)
--         ]
--     <|
--         case dProfile of
--             Desktop ->
--                 [ boldProclamationEl dProfile
--                 , Element.row
--                     [ Element.width Element.fill
--                     , Element.spacing 40
--                     , Element.clip
--                     , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
--                     ]
--                     [ Element.column
--                         [ Element.width Element.fill
--                         , Element.height Element.fill
--                         , Element.clip
--                         ]
--                       <|
--                         [ postFeed
--                             dProfile
--                             donateChecked
--                             blockTimes
--                             now
--                             maybeShowAddressForPostId
--                             (Dict.values posts |> List.concat)
--                         ]
--                     ]
--                 ]
--             Mobile ->
--                 Debug.todo ""


majorSpacing : Int
majorSpacing =
    20



-- [ boldProclamationEl dProfile
-- , tutorialVideo dProfile
-- , conversationAlreadyStartedEl dProfile
-- , case walletUXPhaceInfo of
--     DemoPhaceInfo _ ->
--         web3ConnectButton
--             dProfile
--             [ Element.width Element.fill ]
--             MsgUp
--     _ ->
--         Element.column
--             [ Element.width Element.fill
--             , Element.spacing 10
--             ]
--             [ theme.greenActionButton
--                 dProfile
--                 [ Element.width Element.fill ]
--                 [ "Create a New Post" ]
--                 (MsgUp <|
--                     GotoRoute <|
--                         Routing.Compose <|
--                             Post.TopLevel Post.defaultTopic
--                 )
--             ]
--, infoBlock dProfile
--, conversationAlreadyStartedEl dProfile
-- , topicsBlock dProfile posts
-- , topicsExplainerEl dProfile
--, composeActionBlock dProfile walletUXPhaceInfo
-- ]


postFeed :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Post.Id
    -> List Post.Published
    -> Element Msg
postFeed dProfile donateChecked blockTimes now maybeShowAddressForId listOfPosts =
    let
        posts =
            List.sortBy (feedSortByFunc blockTimes now)
                listOfPosts
                |> List.reverse
                |> List.take 10

        _ =
            Debug.log "l" <| List.length listOfPosts
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacingXY 0 20
        , Element.paddingXY 0 20
        ]
    <|
        List.map
            (previewPost
                dProfile
                donateChecked
                blockTimes
                now
                maybeShowAddressForId
                Nothing
            )
            posts


feedSortByFunc : Dict Int Time.Posix -> Time.Posix -> (Post.Published -> Float)
feedSortByFunc blockTimes now =
    \post ->
        let
            postTimeDefaultZero =
                blockTimes
                    |> Dict.get post.id.block
                    |> Maybe.withDefault (Time.millisToPosix 0)

            age =
                TimeHelpers.sub now postTimeDefaultZero

            ageFactor =
                -- 1 at age zero, falls to 0 when 3 days old
                TimeHelpers.getRatio
                    age
                    (TimeHelpers.mul TimeHelpers.oneDay 90)
                    |> clamp 0 1
                    |> (\ascNum -> 1 - ascNum)

            totalBurned =
                Post.totalBurned (Post.PublishedPost post)
                    |> TokenValue.toFloatWithWarning

            newnessMultiplier =
                1

            -- (ageFactor * 4.0) + 1
        in
        totalBurned * newnessMultiplier


previewPost :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Post.Id
    -> Maybe PostUX.Model
    -> Post.Published
    -> Element Msg
previewPost dProfile donateChecked blockTimes now maybeShowAddressForId maybePostUXModel post =
    Element.map PostUXMsg <|
        PostPreview.view
            dProfile
            donateChecked
            (maybeShowAddressForId == Just post.id)
            blockTimes
            now
            maybePostUXModel
            post



-- homeWalletUX : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
-- homeWalletUX dProfile walletUXPhaceInfo =
--     Element.map MsgUp <|
--         case walletUXPhaceInfo of
--             DemoPhaceInfo demoAddress ->
--                 Element.el
--                     [ Element.pointer
--                     , Element.Events.onClick <| ConnectToWeb3
--                     , Element.Border.rounded 10
--                     , Element.Border.glow
--                         (Element.rgba 1 0 1 0.3)
--                         9
--                     ]
--                 <|
--                     phaceElement
--                         ( 100, 100 )
--                         True
--                         (Eth.Utils.unsafeToAddress demoAddress)
--                         False
--                         (ShowOrHideAddress MorphingPhace)
--                         NoOp
--             UserPhaceInfo ( accountInfo, showAddress ) ->
--                 Element.el
--                     [ Element.Border.rounded 10
--                     , Element.Border.glow
--                         (Element.rgba 0 0.5 1 0.4)
--                         9
--                     ]
--                 <|
--                     phaceElement
--                         ( 100, 100 )
--                         True
--                         accountInfo.address
--                         showAddress
--                         (ShowOrHideAddress UserPhace)
--                         NoOp
