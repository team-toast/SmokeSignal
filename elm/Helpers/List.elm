module Helpers.List exposing (..)


nonEmpty : List a -> Maybe (List a)
nonEmpty l =
    case l of
        [] ->
            Nothing

        _ ->
            Just l
