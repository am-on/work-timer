module Grid exposing (viewGrid)

import Dict
import Html exposing (..)
import Svg
import Svg.Attributes
import Time
import TimeEntry
    exposing
        ( ClockTime
        , TimeEntries
        , TimerState
        , getTodoTime
        )


type alias Grid =
    Dict.Dict ( Int, Int ) Int


addMinutesToGrid : Int -> Int -> Grid -> Int -> Grid
addMinutesToGrid x y grid minutes =
    let
        oldValue =
            Maybe.withDefault 0 (Dict.get ( y, x ) grid)
    in
    Dict.update
        ( y, x )
        (always (Just (oldValue + minutes)))
        grid


timeEntryToBins : Time.Posix -> Time.Posix -> Time.Zone -> Grid -> Grid
timeEntryToBins start stop timezone grid =
    if Time.posixToMillis start >= Time.posixToMillis stop then
        grid

    else
        let
            startHour =
                Time.toHour timezone start

            startMinutes =
                Time.toMinute timezone start

            startSeconds =
                Time.toSecond timezone start

            startMilis =
                Time.toMillis timezone start

            stopMinutes =
                Time.toMinute timezone stop

            -- Position of the current bin
            y =
                startHour

            x =
                startMinutes // 10

            minutesToNextBin =
                10 - modBy 10 startMinutes

            secondInMillis =
                1000

            minuteInMillis =
                60 * secondInMillis

            nextBinStartTime =
                (Time.posixToMillis start + (minutesToNextBin * minuteInMillis))
                    - (startSeconds * secondInMillis)
                    - startMilis
        in
        if nextBinStartTime > Time.posixToMillis stop then
            -- start and stop times are in the same bin
            -- 16:32:20 -> 16:38:30
            addMinutesToGrid x y grid (stopMinutes - startMinutes)

        else
            -- stop time is not in the same bin, continue to next bin
            -- 16:32:20 -> 16:58:30
            timeEntryToBins
                (Time.millisToPosix nextBinStartTime)
                stop
                timezone
                (addMinutesToGrid x y grid minutesToNextBin)


populateGrid : TimeEntries -> Time.Posix -> Time.Zone -> Grid -> Grid
populateGrid entries time timezone grid =
    case entries of
        x :: xs ->
            let
                newGrid =
                    timeEntryToBins x.start (Maybe.withDefault time x.stop) timezone grid
            in
            populateGrid xs time timezone newGrid

        [] ->
            grid


viewGrid : TimerState -> TimeEntries -> Time.Posix -> Time.Zone -> Html msg
viewGrid timerState timeEntries time timezone =
    let
        hours =
            List.range 6 22

        minutesBins =
            List.range 0 5

        keys =
            List.map2 Tuple.pair
                (hours |> List.map (\hour -> List.repeat 6 hour) |> List.concat)
                (hours |> List.map (\_ -> minutesBins) |> List.concat)

        grid =
            keys
                |> List.map (\key -> ( key, 0 ))
                |> Dict.fromList
                |> populateGrid timeEntries time timezone

        textLabel =
            \y x label ->
                Svg.text_
                    [ Svg.Attributes.x (String.fromInt x)
                    , Svg.Attributes.y (String.fromInt y)
                    , Svg.Attributes.fontSize "16"
                    , Svg.Attributes.class "text-grey-dark fill-current"
                    ]
                    [ text label ]

        labelsFrom =
            hours
                |> List.map
                    (\hour ->
                        let
                            y =
                                ((hour - 6) * 35) + 21

                            x =
                                5

                            label =
                                String.padLeft 2 '0' (String.fromInt hour) ++ ":00"
                        in
                        textLabel y x label
                    )

        labelsTo =
            hours
                |> List.map
                    (\hour ->
                        let
                            y =
                                ((hour - 6) * 35) + 21

                            x =
                                265

                            label =
                                String.padLeft 2 '0' (String.fromInt hour) ++ ":59"
                        in
                        textLabel y x label
                    )

        timeBinRect =
            \y x minutes ->
                let
                    opacity =
                        Basics.max 0.2 (toFloat minutes / 10.0)

                    color =
                        if minutes > 5 then
                            "#38c172"

                        else if minutes > 0 then
                            "#64d5ca"

                        else
                            "#b8c2cc"
                in
                Svg.rect
                    [ Svg.Attributes.x (String.fromInt x)
                    , Svg.Attributes.y (String.fromInt y)
                    , Svg.Attributes.width "30"
                    , Svg.Attributes.height "30"
                    , Svg.Attributes.rx "2"
                    , Svg.Attributes.ry "2"
                    , Svg.Attributes.fill color
                    , Svg.Attributes.opacity (String.fromFloat opacity)
                    ]
                    []

        timeBins =
            grid
                |> Dict.toList
                |> List.map (\( ( y, x ), minutes ) -> timeBinRect ((y - 6) * 35) (50 + x * 35) minutes)

        clockTime : ClockTime
        clockTime =
            { hours = Time.toHour timezone time
            , minutes = Time.toMinute timezone time
            , seconds = Time.toSecond timezone time
            }

        currentTimeIndicator =
            Svg.rect
                [ Svg.Attributes.x (String.fromFloat (50 + toFloat clockTime.minutes / 60 * 205))
                , Svg.Attributes.y (String.fromInt (-2 + (clockTime.hours - 6) * 35))
                , Svg.Attributes.width "2"
                , Svg.Attributes.height "34"
                , Svg.Attributes.rx "2"
                , Svg.Attributes.ry "2"
                , Svg.Attributes.fill "#e3342f"
                , Svg.Attributes.opacity "1"
                ]
                []

        endTime =
            Time.posixToMillis time
                + (getTodoTime timerState timeEntries time * 1000)
                |> Time.millisToPosix

        endClockTime =
            { hours = Time.toHour timezone endTime
            , minutes = Time.toMinute timezone endTime
            , seconds = Time.toSecond timezone endTime
            }

        stopTimeIndicator =
            Svg.rect
                [ Svg.Attributes.x (String.fromFloat (50 + toFloat endClockTime.minutes / 60 * 205))
                , Svg.Attributes.y (String.fromInt (-2 + (endClockTime.hours - 6) * 35))
                , Svg.Attributes.width "2"
                , Svg.Attributes.height "34"
                , Svg.Attributes.rx "2"
                , Svg.Attributes.ry "2"
                , Svg.Attributes.fill "black"
                , Svg.Attributes.opacity "1"
                ]
                []
    in
    Svg.svg
        [ Svg.Attributes.width "305"
        , Svg.Attributes.height "600"
        , Svg.Attributes.viewBox "0 0 305 600"
        ]
        (timeBins ++ labelsFrom ++ labelsTo ++ [ currentTimeIndicator, stopTimeIndicator ])
