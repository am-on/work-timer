module MonthView exposing (viewMonth)

import Html exposing (..)
import Time
import TimeEntry exposing (TimeEntries)
import TimeHelpers exposing (viewWorkedTime)


viewMonth : TimeEntries -> Time.Posix -> Html msg
viewMonth timeEntries time =
    div [] [ text (viewWorkedTime timeEntries time) ]
