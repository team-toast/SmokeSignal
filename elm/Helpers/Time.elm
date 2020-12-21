module Helpers.Time exposing (..)

import BigInt exposing (BigInt)
import Helpers.BigInt as BigIntHelpers
import Time


add : Time.Posix -> Time.Posix -> Time.Posix
add t1 t2 =
    t1
        |> Time.posixToMillis
        |> (+) (Time.posixToMillis t2)
        |> Time.millisToPosix


sub : Time.Posix -> Time.Posix -> Time.Posix
sub t1 t2 =
    t2
        |> Time.posixToMillis
        |> (-) (Time.posixToMillis t1)
        |> Time.millisToPosix


mul : Time.Posix -> Int -> Time.Posix
mul t i =
    t
        |> Time.posixToMillis
        |> (*) i
        |> Time.millisToPosix


negativeToZero : Time.Posix -> Time.Posix
negativeToZero t =
    if Time.posixToMillis t < 0 then
        Time.millisToPosix 0

    else
        t


getRatio : Time.Posix -> Time.Posix -> Float
getRatio t1 t2 =
    toFloat (Time.posixToMillis t1) / toFloat (Time.posixToMillis t2)


isNegative : Time.Posix -> Bool
isNegative t =
    Time.posixToMillis t < 0


secondsBigIntToMaybePosix : BigInt -> Maybe Time.Posix
secondsBigIntToMaybePosix bigint =
    BigInt.toString bigint
        |> String.toInt
        |> Maybe.map (\t -> Time.millisToPosix (t * 1000))


daysStrToMaybePosix : String -> Maybe Time.Posix
daysStrToMaybePosix timeStr =
    let
        daysToMillis days =
            days * 24 * 60 * 60 * 1000
    in
    timeStr
        |> String.toFloat
        |> Maybe.map daysToMillis
        |> Maybe.map floor
        |> Maybe.map Time.millisToPosix


posixToSeconds : Time.Posix -> Int
posixToSeconds t =
    Time.posixToMillis t // 1000


secondsToPosix : Int -> Time.Posix
secondsToPosix s =
    Time.millisToPosix (s * 1000)


secondsBigIntToPosixWithWarning : BigInt -> Time.Posix
secondsBigIntToPosixWithWarning =
    BigIntHelpers.toIntWithWarning
        >> (\secs -> secs * 1000)
        >> Time.millisToPosix


posixToMillisBigInt : Time.Posix -> BigInt
posixToMillisBigInt t =
    Time.posixToMillis t
        |> BigInt.fromInt


posixToSecondsBigInt : Time.Posix -> BigInt
posixToSecondsBigInt t =
    BigInt.div (posixToMillisBigInt t) (BigInt.fromInt 1000)


toString : Time.Posix -> String
toString t =
    (posixToSeconds t
        |> String.fromInt
    )
        ++ " seconds"


compare : Time.Posix -> Time.Posix -> Order
compare t1 t2 =
    Basics.compare
        (Time.posixToMillis t1)
        (Time.posixToMillis t2)


type alias HumanReadableInterval =
    { years : Int
    , months : Int
    , days : Int
    , hours : Int
    , min : Int
    , sec : Int
    }



--ignores some maybes, because we never divmod by zero.


toHumanReadableInterval : Time.Posix -> HumanReadableInterval
toHumanReadableInterval t =
    let
        defaultToZeros : Maybe ( BigInt, BigInt ) -> ( BigInt, BigInt )
        defaultToZeros =
            Maybe.withDefault ( BigInt.fromInt 0, BigInt.fromInt 0 )

        secsInYears =
            posixToSecondsBigInt t
    in
    BigInt.divmod secsInYears (BigInt.fromInt <| 60 * 60 * 24 * 365)
        |> defaultToZeros
        |> (\( years, secsInMonths ) ->
                BigInt.divmod secsInMonths (BigInt.fromInt <| 60 * 60 * 24 * 30)
                    |> defaultToZeros
                    |> (\( months, secsInDays ) ->
                            BigInt.divmod secsInDays (BigInt.fromInt <| 60 * 60 * 24)
                                |> defaultToZeros
                                |> (\( days, secsInHours ) ->
                                        BigInt.divmod secsInHours (BigInt.fromInt <| 60 * 60)
                                            |> defaultToZeros
                                            |> (\( hours, secsInMin ) ->
                                                    BigInt.divmod secsInMin (BigInt.fromInt 60)
                                                        |> defaultToZeros
                                                        |> (\( min, sec ) ->
                                                                HumanReadableInterval
                                                                    (BigIntHelpers.toIntWithWarning years)
                                                                    (BigIntHelpers.toIntWithWarning months)
                                                                    (BigIntHelpers.toIntWithWarning days)
                                                                    (BigIntHelpers.toIntWithWarning hours)
                                                                    (BigIntHelpers.toIntWithWarning min)
                                                                    (BigIntHelpers.toIntWithWarning sec)
                                                           )
                                               )
                                   )
                       )
           )


toConciseIntervalString : Time.Posix -> String
toConciseIntervalString t =
    let
        hri =
            toHumanReadableInterval t
    in
    if hri.years > 0 then
        String.fromInt hri.years ++ "y " ++ String.fromInt hri.months ++ "m"

    else if hri.months > 0 then
        String.fromInt hri.months ++ "m " ++ String.fromInt hri.days ++ "d"

    else if hri.days > 0 then
        String.fromInt hri.days ++ "d " ++ String.fromInt hri.hours ++ "h"

    else if hri.hours > 0 then
        String.fromInt hri.hours ++ "h " ++ String.fromInt hri.min ++ "m"

    else if hri.min > 0 then
        String.fromInt hri.min ++ "m " ++ String.fromInt hri.sec ++ "s"

    else
        String.fromInt hri.sec ++ "s"


roundToSingleUnit : Time.Posix -> String
roundToSingleUnit t =
    let
        hri =
            toHumanReadableInterval t

        ( num, unitStr ) =
            if hri.years > 0 then
                ( hri.years, "year" )

            else if hri.months > 0 then
                ( hri.months, "month" )

            else if hri.days > 0 then
                ( hri.days, "day" )

            else if hri.hours > 0 then
                ( hri.hours, "hour" )

            else if hri.min > 0 then
                ( hri.min, "minute" )

            else
                ( hri.sec, "second" )

        pluralizedUnitStr =
            if num > 1 then
                unitStr ++ "s"

            else
                unitStr
    in
    String.fromInt num ++ " " ++ pluralizedUnitStr


oneSecond : Time.Posix
oneSecond =
    secondsToPosix 1


oneMinute : Time.Posix
oneMinute =
    secondsToPosix 60


oneHour : Time.Posix
oneHour =
    secondsToPosix <| 60 * 60


oneDay : Time.Posix
oneDay =
    secondsToPosix <| 60 * 60 * 24


oneWeek : Time.Posix
oneWeek =
    secondsToPosix <| 60 * 60 * 24 * 7


oneYear : Time.Posix
oneYear =
    secondsToPosix 31557600


weekdayToShortString : Time.Weekday -> String
weekdayToShortString wd =
    case wd of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


monthToShortString : Time.Month -> String
monthToShortString m =
    case m of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


monthToInt : Time.Month -> Int
monthToInt m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
