module ComposeUX.Types exposing (..)

import Element exposing (Element)
import Eth.Types exposing (Address, TxHash)
import Http
import Post
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)


type alias Model =
    { now : Time.Posix
    , context : Context
    , content : Content
    , daiInput : String
    , showPreviewOnMobile : Bool
    , lastInputChangedTime : Time.Posix
    , renderNeeded : Bool
    , renderedPreview : Maybe (Element Never)
    }


type Msg
    = MsgUp MsgUp
    | BodyInputChanged String
    | DaiInputChanged String
    | MobilePreviewToggle
    | Tick Time.Posix


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


updateContext : Context -> Model -> Model
updateContext context m =
    { m | context = context }


updateBody : String -> Model -> Model
updateBody body m =
    let
        prevContent =
            m.content
    in
    { m
        | content =
            { prevContent
                | body = body
            }
    }


updateDaiInput : String -> Model -> Model
updateDaiInput input m =
    { m | daiInput = input }


type alias CheckedMaybeValidInputs =
    { content : Maybe Content
    , burnAndDonateAmount : Maybe (Result String ( TokenValue, TokenValue ))
    }


validateInputs : Bool -> Model -> CheckedMaybeValidInputs
validateInputs donateChecked composeModel =
    { content =
        if composeModel.content.body == "" then
            Nothing

        else
            Just composeModel.content
    , burnAndDonateAmount =
        validateBurnAmount composeModel.daiInput
            |> Maybe.map
                (Result.map
                    (\burnAmount ->
                        ( burnAmount
                        , if donateChecked then
                            TokenValue.div burnAmount 100

                          else
                            TokenValue.zero
                        )
                    )
                )
    }


validateBurnAmount : String -> Maybe (Result String TokenValue)
validateBurnAmount input =
    if input == "" then
        Nothing

    else
        Just
            (TokenValue.fromString input
                |> Result.fromMaybe "Invalid burn amount"
                |> Result.andThen
                    (\tv ->
                        if TokenValue.compare tv TokenValue.zero == GT then
                            Ok tv

                        else
                            Err "Minimum amount is 0.000000000000000001 DAI"
                    )
            )


subscriptions : Sub Msg
subscriptions =
    Time.every 200 Tick
