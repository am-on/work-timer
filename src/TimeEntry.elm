module TimeEntry exposing
    ( ClockTime
    , TimeEntries
    , TimeEntry
    , getEntryDuration
    , getTodoTime
    , getWorkedTime
    )

import Time


requiredHours : Int
requiredHours =
    -- 7.5 hours in seconds
    7 * 60 * 60 + 1 * 30 * 60


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


getWorkedTime : TimeEntries -> Time.Posix -> Int
getWorkedTime timeEntries time =
    -- Get worked time in seconds
    List.map (\entry -> getEntryDuration entry time) timeEntries
        |> List.sum


getTodoTime : TimeEntries -> Time.Posix -> Int
getTodoTime timeEntries time =
    let
        done =
            getWorkedTime timeEntries time
    in
    requiredHours - done


getEntryDuration : TimeEntry -> Time.Posix -> Int
getEntryDuration entry time =
    if entry.duration >= 0 then
        entry.duration

    else
        let
            currentTime =
                Time.posixToMillis time // 1000
        in
        -- If the time entry is currently running, the duration attribute
        -- contains a negative value, denoting the start of the time entry in
        -- seconds since epoch (Jan 1 1970). The correct duration can be
        -- calculated as current_time + duration, where current_time is the
        -- current time in seconds since epoch.
        --https://github.com/toggl/toggl_api_docs/issues/16#issuecomment-17919490
        currentTime + entry.duration
