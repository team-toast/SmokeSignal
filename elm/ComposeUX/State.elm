module ComposeUX.State exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View
import ComposeUX.Types exposing (..)
import Contracts.Dai as Dai
import Contracts.SmokeSignal as SSContract
import Dict exposing (Dict)
import Element exposing (Element)
import Eth
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Utils
import Helpers.Time as TimeHelpers
import Post
import Time
import UserNotice as UN


init : Time.Posix -> Context -> Model
init now context =
    { now = now
    , context = context
    , content = Post.justBodyContent ""
    , daiInput = ""
    , showPreviewOnMobile = False
    , lastInputChangedTime = Time.millisToPosix 0
    , renderNeeded = False
    , renderedPreview = Nothing
    }


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        BodyInputChanged input ->
            justModelUpdate
                ({ prevModel
                    | renderNeeded = True
                    , lastInputChangedTime = prevModel.now
                 }
                    |> updateBody input
                )

        DaiInputChanged input ->
            justModelUpdate
                (prevModel |> updateDaiInput input)

        MobilePreviewToggle ->
            justModelUpdate
                { prevModel
                    | showPreviewOnMobile =
                        not prevModel.showPreviewOnMobile
                }

        Tick time ->
            let
                msecSinceEdit =
                    TimeHelpers.sub
                        prevModel.now
                        prevModel.lastInputChangedTime
                        |> Time.posixToMillis
            in
            if prevModel.renderNeeded && (msecSinceEdit > 300) then
                justModelUpdate
                    { prevModel
                        | now = time
                        , renderedPreview =
                            renderPreviewIfNonEmpty prevModel.content
                        , renderNeeded = False
                    }

            else
                justModelUpdate
                    { prevModel
                        | now = time
                    }

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]


resetModel : Model -> Model
resetModel model =
    { model
        | content = Post.justBodyContent ""
        , showPreviewOnMobile = False
        , daiInput = ""
        , renderedPreview = Nothing
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        []


renderPreviewIfNonEmpty : Content -> Maybe (Element Never)
renderPreviewIfNonEmpty content =
    if Post.contentIsEmpty content then
        Nothing

    else
        Just <| Common.View.renderContentOrError content
