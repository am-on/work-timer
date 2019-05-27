module MonthView exposing (viewMonth)

import Html exposing (..)
import Time
import TimeEntry
    exposing
        ( ClockTime
        , DateTime
        , TimeEntries
        , TimeEntry
        , getEntryDuration
        , getTodoTime
        , getWorkedTime
        , myTimeZone
        )
import TimeHelpers
    exposing
        ( toClockTime
        , todoTime
        , viewClockTime
        , viewShortClockTime
        , viewWorkedTime
        )


viewMonth : TimeEntries -> Time.Posix -> Html msg
viewMonth timeEntries time =
    div [] [ text (viewWorkedTime timeEntries time) ]
