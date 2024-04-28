module MonthView exposing (viewMonth)

import Html exposing (..)
import Time
import TimeEntry exposing (TimeEntries, TimerState)
import TimeHelpers exposing (viewWorkedTime)


viewMonth : TimerState -> TimeEntries -> Time.Posix -> Html msg
viewMonth timerState timeEntries time =
    div [] [ text (viewWorkedTime timerState timeEntries time) ]
