module Routing exposing (..)

import Common.Types exposing (..)
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Post exposing (Post)
import Result.Extra
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type Route
    = InitialBlank
    | Home
    | ViewAll
    | ViewPost Post.Id
    | ViewTopic String
    | NotFound String


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map ViewAll (Parser.s "all")
        , Parser.map viewPostOrParseFail (Parser.s "viewpost" <?> postIdInfoParser)
        , Parser.map ViewTopic (Parser.s "topic" </> Parser.string)
        ]


viewPostOrParseFail : Result String Post.Id -> Route
viewPostOrParseFail =
    Result.Extra.unpack
        NotFound
        ViewPost


routeToString : Route -> String
routeToString route =
    case route of
        InitialBlank ->
            Builder.absolute
                [ "#" ]
                []

        Home ->
            Builder.absolute
                [ "#" ]
                []

        ViewAll ->
            Builder.absolute
                [ "#", "all" ]
                []

        ViewPost postId ->
            Builder.absolute
                [ "#", "viewpost" ]
                (encodePostIdInfoQueryParameters postId)

        ViewTopic topic ->
            Builder.absolute
                [ "#", "topic", topic ]
                []

        NotFound _ ->
            Builder.absolute
                [ "#" ]
                []


routeToFullDotEthUrlString : Route -> String
routeToFullDotEthUrlString route =
    "https://smokesignal.eth" ++ routeToString route


postIdInfoParser : Query.Parser (Result String Post.Id)
postIdInfoParser =
    Query.map2
        (Result.map2 Post.Id)
        (Query.int "block"
            |> Query.map (Result.fromMaybe "Can't interpret 'block'")
        )
        (hexQueryParser "hash")


encodePostIdInfoQueryParameters : Post.Id -> List Builder.QueryParameter
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
    Maybe.withDefault (NotFound "url not found") (Parser.parse routeParser url)
