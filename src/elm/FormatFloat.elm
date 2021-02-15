module FormatFloat exposing (autoFormatFloat, formatFloat)

import FormatNumber
import FormatNumber.Locales exposing (usLocale)


formatFloat : Int -> Float -> String
formatFloat numDecimals =
    FormatNumber.format
        { usLocale
            | decimals = FormatNumber.Locales.Exact numDecimals
        }


autoFormatFloat : Float -> String
autoFormatFloat f =
    let
        magnitude =
            floor <| logBase 10 f + 1

        numDecimals =
            max
                (3 - magnitude)
                0
    in
    formatFloat
        numDecimals
        f
