module Grid exposing (viewGrid)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes
import Time
import TimeEntry exposing (ClockTime, DateTime, TimeEntries, getTodoTime, myTimeZone)


type alias Grid =
    Dict.Dict ( Int, Int ) Int


timeEntryToBins : DateTime -> DateTime -> Grid -> Grid
timeEntryToBins start end grid =
    if start.hours > end.hours || (start.hours == end.hours && start.minutes >= end.minutes) then
        grid

    else
        let
            y =
                start.hours

            x =
                start.minutes // 10

            binValue =
                10 - modBy 10 start.minutes

            minutes =
                start.minutes + binValue

            newStart =
                { year = 0
                , month = 0
                , day = 0
                , hours = start.hours + (minutes // 60)
                , minutes = modBy 60 minutes
                , seconds = start.seconds
                }
        in
        if newStart.hours > end.hours || (newStart.hours == end.hours && newStart.minutes >= end.minutes) then
            Dict.update ( y, x ) (always (Just (end.minutes - start.minutes))) grid

        else
            timeEntryToBins newStart end (Dict.update ( y, x ) (always (Just binValue)) grid)


populateGrid : TimeEntries -> Time.Posix -> Grid -> Grid
populateGrid entries time grid =
    case entries of
        x :: xs ->
            let
                currentTime : DateTime
                currentTime =
                    { year = 0
                    , month = 0
                    , day = Time.toDay Time.utc time
                    , hours = Time.toHour Time.utc time + myTimeZone
                    , minutes = Time.toMinute Time.utc time
                    , seconds = Time.toSecond Time.utc time
                    }

                newGrid =
                    timeEntryToBins x.start (Maybe.withDefault currentTime x.stop) grid
            in
            populateGrid xs time newGrid

        [] ->
            grid


viewGrid : TimeEntries -> Time.Posix -> Html msg
viewGrid timeEntries time =
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
                |> populateGrid timeEntries time

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
            { hours = Time.toHour Time.utc time + myTimeZone
            , minutes = Time.toMinute Time.utc time
            , seconds = Time.toSecond Time.utc time
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
                + (getTodoTime timeEntries time * 1000)
                |> Time.millisToPosix

        endClockTime =
            { hours = Time.toHour Time.utc endTime + myTimeZone
            , minutes = Time.toMinute Time.utc endTime
            , seconds = Time.toSecond Time.utc endTime
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
