module TimeHelpers exposing
    ( toClockTime
    , todoTime
    , viewClockTime
    , viewShortClockTime
    , viewShortPosixTime
    , viewWorkedTime
    )

import Time
import TimeEntry
    exposing
        ( ClockTime
        , TimeEntries
        , TimerState(..)
        , getTodoTime
        , getWorkedTime
        )


viewShortPosixTime : Time.Posix -> Time.Zone -> String
viewShortPosixTime time timezone =
    let
        hours =
            Time.toHour timezone time

        minutes =
            Time.toMinute timezone time
    in
    String.padLeft 2 '0' (String.fromInt hours)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt minutes)


viewShortClockTime : ClockTime -> String
viewShortClockTime time =
    String.padLeft 2 '0' (String.fromInt time.hours)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt time.minutes)


viewClockTime : ClockTime -> String
viewClockTime time =
    String.padLeft 2 '0' (String.fromInt time.hours)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt time.minutes)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt time.seconds)


toClockTime : Int -> ClockTime
toClockTime seconds =
    let
        clockTime : ClockTime
        clockTime =
            { hours = seconds // (60 * 60)
            , minutes = remainderBy 60 (seconds // 60)
            , seconds = remainderBy 60 seconds
            }
    in
    clockTime


viewWorkedTime : TimerState -> TimeEntries -> Time.Posix -> String
viewWorkedTime timerState timeEntries time =
    getWorkedTime timerState timeEntries time
        |> toClockTime
        |> viewClockTime


todoTime : TimerState -> TimeEntries -> Time.Posix -> String
todoTime timerState timeEntries time =
    let
        todo =
            getTodoTime timerState timeEntries time

        overtime =
            todo < 0

        viewTime =
            abs todo
                |> toClockTime
                |> viewClockTime
    in
    if overtime then
        "+ " ++ viewTime

    else
        viewTime
