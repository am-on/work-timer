module TimeEntry exposing
    ( ClockTime
    , TimeEntries
    , TimeEntry
    , TimerState(..)
    , getEntryDuration
    , getTodoTime
    , getWorkedTime
    )

import Time


requiredHours : Int
requiredHours =
    -- 7.5 hours in seconds
    7 * 60 * 60 + 1 * 30 * 60


type TimerState
    = Stopped
    | SwitchingToRunning
    | Running
    | SwitchingToStopped Time.Posix


type alias ClockTime =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


type alias TimeEntry =
    { id : Int
    , start : Time.Posix
    , stop : Maybe Time.Posix
    , duration : Int
    }


type alias TimeEntries =
    List TimeEntry


getWorkedTime : TimerState -> TimeEntries -> Time.Posix -> Int
getWorkedTime timerState timeEntries time =
    -- Get worked time in seconds
    List.map (\entry -> getEntryDuration timerState entry time) timeEntries
        |> List.sum


getTodoTime : TimerState -> TimeEntries -> Time.Posix -> Int
getTodoTime timerState timeEntries time =
    let
        done =
            getWorkedTime timerState timeEntries time
    in
    requiredHours - done


getEntryDuration : TimerState -> TimeEntry -> Time.Posix -> Int
getEntryDuration timerState entry time =
    if entry.duration >= 0 then
        entry.duration

    else
        let
            -- Get current time in seconds since epoch
            currentTime : Int
            currentTime =
                Time.posixToMillis time // 1000

            entryEnd : Int
            entryEnd =
                case timerState of
                    SwitchingToStopped stopTime ->
                        Time.posixToMillis stopTime // 1000

                    _ ->
                        currentTime

            -- Get entry start time in seconds since epoch
            entryStart : Int
            entryStart =
                Time.posixToMillis entry.start // 1000
        in
        entryEnd - entryStart
