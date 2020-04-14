module Routing exposing (..)

import CommonTypes exposing (..)
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type Route
    = InitialBlank
    | Default
    | ViewPost (Result String PostId)
    | ViewTopic String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Default Parser.top
        , Parser.map ViewPost (Parser.s "viewpost" <?> postIdInfoParser)
        , Parser.map ViewTopic (Parser.s "topic" </> Parser.string)
        ]


routeToString : Route -> String
routeToString route =
    case route of
        InitialBlank ->
            Builder.absolute
                [ "#" ]
                []

        Default ->
            Builder.absolute
                [ "#" ]
                []

        ViewPost postIdInfoResult ->
            case postIdInfoResult of
                Ok postIdInfo ->
                    Builder.absolute
                        [ "#", "viewpost" ]
                        (encodePostIdInfoQueryParameters postIdInfo)

                Err _ ->
                    Builder.absolute
                        [ "#" ]
                        []

        ViewTopic topic ->
            Builder.absolute
                [ "#", "topic", topic ]
                []

        NotFound ->
            Builder.absolute
                [ "#" ]
                []


routeToFullDotEthUrlString : Route -> String
routeToFullDotEthUrlString route =
    "https://smokesignal.eth" ++ routeToString route


postIdInfoParser : Query.Parser (Result String PostId)
postIdInfoParser =
    Query.map2
        (Result.map2 PostId)
        (Query.int "block"
            |> Query.map (Result.fromMaybe "Can't interpret 'block'")
        )
        (hexQueryParser "hash")


encodePostIdInfoQueryParameters : PostId -> List Builder.QueryParameter
encodePostIdInfoQueryParameters postIdInfo =
    [ Builder.string "block" (String.fromInt postIdInfo.block)
    , Builder.string "hash" (Eth.Utils.hexToString postIdInfo.messageHash)
    ]


hexQueryParser : String -> Query.Parser (Result String Hex)
hexQueryParser label =
    Query.string label
        |> Query.map (Result.fromMaybe "Can't find 'hash'")
        |> Query.map (Result.andThen Eth.Utils.toHex)


addressParser : Parser (Address -> a) a
addressParser =
    Parser.custom
        "ADDRESS"
        (Eth.Utils.toAddress >> Result.toMaybe)


urlToRoute : Url -> Route
urlToRoute url =
    Maybe.withDefault NotFound (Parser.parse routeParser url)
