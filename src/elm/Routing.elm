module Routing exposing (addressParser, encodePostIdQueryParameters, encodeTopic, hexQueryParser, postIdQueryParser, routeParser, topicParser, urlToRoute, viewToUrlString)

import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Maybe.Extra
import Result.Extra
import Types exposing (..)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map RouteHome Parser.top
        , Parser.s "post"
            <?> postIdQueryParser
            |> Parser.map (Maybe.Extra.unwrap RouteMalformedPostId RouteViewPost)
        , Parser.s "topic"
            </> topicParser
            |> Parser.map (Maybe.Extra.unwrap RouteMalformedTopic RouteViewTopic)
        ]


viewToUrlString : String -> View -> String
viewToUrlString basePath view =
    basePath
        ++ (case view of
                ViewHome ->
                    Builder.relative
                        [ "#!" ]
                        []

                ViewPost postId ->
                    Builder.relative
                        [ "#!", "post" ]
                        (encodePostIdQueryParameters postId)

                ViewTopic topic ->
                    Builder.relative
                        [ "#!", "topic", encodeTopic topic ]
                        []

                ViewCompose _ ->
                    Builder.relative
                        [ "#!" ]
                        []
           )



-- routeToFullDotEthUrlString : Route -> String
-- routeToFullDotEthUrlString route =
--     routeToString "https://smokesignal.eth/" route
-- contextParser : Parser (Result String Context -> a) a
-- contextParser =
--     Parser.oneOf
--         [ (Parser.s "re" <?> postIdQueryParser)
--             |> Parser.map (Result.map Reply)
--         , (Parser.s "topic" </> topicParser)
--             |> Parser.map (Result.fromMaybe "Couldn't parse topic")
--             |> Parser.map (Result.map TopLevel)
--         ]
-- encodeContextPaths : Context -> List String
-- encodeContextPaths context =
--     case context of
--         Reply _ ->
--             [ "re" ]
--         TopLevel topic ->
--             [ "topic", encodeTopic topic ]


encodeTopic : String -> String
encodeTopic =
    Url.percentEncode


topicParser : Parser (Maybe String -> a) a
topicParser =
    Parser.string
        |> Parser.map Url.percentDecode



-- encodePostContextQueryParams : Context -> List Builder.QueryParameter
-- encodePostContextQueryParams context =
--     case context of
--         Reply postId ->
--             encodePostIdQueryParameters postId
--         TopLevel _ ->
--             []
-- encodeContextQueryParams : Context -> List Builder.QueryParameter
-- encodeContextQueryParams context =
--     context
--         |> encodePostContextQueryParams


postIdQueryParser : Query.Parser (Maybe PostId)
postIdQueryParser =
    Query.map2
        (Maybe.map2 PostId)
        (Query.int "block")
        (hexQueryParser "hash")


encodePostIdQueryParameters : PostId -> List Builder.QueryParameter
encodePostIdQueryParameters postIdInfo =
    [ Builder.string "block" (String.fromInt postIdInfo.block)
    , Builder.string "hash" (Eth.Utils.hexToString postIdInfo.messageHash)
    ]


hexQueryParser : String -> Query.Parser (Maybe Hex)
hexQueryParser label =
    Query.string label
        |> Query.map (Maybe.andThen (Eth.Utils.toHex >> Result.toMaybe))


addressParser : Parser (Address -> a) a
addressParser =
    Parser.custom
        "ADDRESS"
        (Eth.Utils.toAddress >> Result.toMaybe)


urlToRoute : Url -> Route
urlToRoute url =
    Maybe.withDefault RouteInvalid (Parser.parse routeParser url)
