module MaybeDebugLog exposing (maybeDebugLog)

-- Changed depending on whether building for prod or debug


maybeDebugLog : a -> b -> b
maybeDebugLog _ a =
    -- Debug.log s a
    a
