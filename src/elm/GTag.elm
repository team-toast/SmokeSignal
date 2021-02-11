port module GTag exposing (GTagData, gTagOut)

import Json.Decode
import Json.Encode


type alias GTagData =
    { event : String
    , category : Maybe String
    , label : Maybe String
    , value : Maybe Int
    }


encodeGTag :
    GTagData
    -> Json.Decode.Value
encodeGTag gtag =
    Json.Encode.object
        [ ( "event", Json.Encode.string gtag.event )
        , ( "category"
          , gtag.category
                |> Maybe.withDefault "[none]"
                -- Crucial note: empty strings for category seems to silently invalidate the gtag completely
                |> Json.Encode.string
          )
        , ( "label"
          , gtag.label
                |> Maybe.withDefault ""
                |> Json.Encode.string
          )
        , ( "value"
          , gtag.value
                |> Maybe.withDefault 0
                |> Json.Encode.int
          )
        ]


gTagOut : GTagData -> Cmd msg
gTagOut =
    encodeGTag >> gTagOutPort


port gTagOutPort : Json.Encode.Value -> Cmd msg
