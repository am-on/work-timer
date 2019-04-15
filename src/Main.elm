module Main exposing (ApiState(..), Model, Msg(..), TimeEntry, getTimeEntries, init, listOfRecordsDecoder, main, subscriptions, timeEntryDecoder, update, view, viewData, viewEntry)

import Array
import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, andThen, at, field, int, list, map4, maybe, string, succeed)
import Json.Encode
import Svg
import Svg.Attributes
import Time exposing (..)



-- CONST


myTimeZone : Int
myTimeZone =
    2



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type TimerState
    = Stopped
    | Running


type ApiState
    = Failure
    | Loading
    | Success


type View
    = DailyList
    | DailyGrid
    | WeekView
    | MonthView


type alias TimeEntries =
    List TimeEntry


type alias Model =
    { apiState : ApiState
    , timerState : TimerState
    , timeEntries : TimeEntries
    , time : Time.Posix
    , apiEndpoint : String
    , apiAuth : String
    , view : View
    }


type alias Flags =
    { apiEndpoint : String
    , apiAuth : String
    , time : Int
    }


type alias ClockTime =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


type alias DateTime =
    { year : Int
    , month : Int
    , day : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { apiState = Loading
            , timerState = Stopped
            , timeEntries = []
            , time = Time.millisToPosix flags.time
            , apiEndpoint = flags.apiEndpoint
            , apiAuth = flags.apiAuth
            , view = DailyGrid
            }
    in
    ( model, getTimeEntries model )



-- UPDATE


type Msg
    = Refresh
    | StartTimer
    | StopTimer
    | RefreshAPI Time.Posix
    | GotTimeEntries (Result Http.Error TimeEntries)
    | StartedTimer (Result Http.Error TimeEntry)
    | StoppedTimer (Result Http.Error TimeEntry)
    | Tick Time.Posix
    | ChangedView View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( { model | apiState = Loading, timeEntries = [] }, getTimeEntries model )

        StartTimer ->
            ( model, startTimer model )

        StopTimer ->
            ( model, stopTimer model )

        RefreshAPI _ ->
            ( model, getTimeEntries model )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        GotTimeEntries (Ok entries) ->
            let
                newModel =
                    { model | apiState = Success, timeEntries = entries }
            in
            ( { newModel | timerState = getTimerState newModel }, Cmd.none )

        GotTimeEntries (Err _) ->
            ( { model | apiState = Failure }, Cmd.none )

        StartedTimer (Ok _) ->
            let
                newModel =
                    { model | timerState = Running }
            in
            ( newModel, getTimeEntries newModel )

        StartedTimer (Err _) ->
            ( model, Cmd.none )

        StoppedTimer (Ok _) ->
            let
                newModel =
                    { model | timerState = Stopped }
            in
            ( newModel, getTimeEntries newModel )

        StoppedTimer (Err _) ->
            ( model, Cmd.none )

        ChangedView newView ->
            ( { model | view = newView }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 5000 RefreshAPI
        , Time.every 1000 Tick
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewData model
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdn.jsdelivr.net/npm/tailwindcss/dist/tailwind.min.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.8.1/css/all.min.css" ] []
        ]


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


populateGrid : TimeEntries -> Model -> Grid -> Grid
populateGrid entries model grid =
    case entries of
        x :: xs ->
            let
                currentTime : DateTime
                currentTime =
                    { year = 0
                    , month = 0
                    , day = toDay utc model.time
                    , hours = toHour utc model.time + myTimeZone
                    , minutes = toMinute utc model.time
                    , seconds = toSecond utc model.time
                    }

                newGrid =
                    timeEntryToBins x.start (Maybe.withDefault currentTime x.stop) grid
            in
            populateGrid xs model newGrid

        [] ->
            grid


viewSvg : Model -> Html Msg
viewSvg model =
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
                |> populateGrid model.timeEntries model

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
            { hours = toHour utc model.time + myTimeZone
            , minutes = toMinute utc model.time
            , seconds = toSecond utc model.time
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
    in
    Svg.svg
        [ Svg.Attributes.width "305"
        , Svg.Attributes.height "590"
        , Svg.Attributes.viewBox "0 0 305 590"
        ]
        (timeBins ++ labelsFrom ++ labelsTo ++ [ currentTimeIndicator ])


viewData : Model -> Html Msg
viewData model =
    case model.apiState of
        Failure ->
            div []
                [ text "I could not get data. "
                , button [ onClick Refresh ] [ text "Try Again!" ]
                , p [] [ text (getDate model.time) ]
                ]

        Loading ->
            text "Loading..."

        Success ->
            div []
                [ div [ class "bg-indigo-darker" ]
                    [ div [ class "container mx-auto text-center pt-20" ]
                        [ h1 [ class "text-white font-normal text-5xl pb-5" ]
                            [ text (todoTime model) ]
                        , p [ class "text-white font-normal pb-5" ]
                            [ text ("(" ++ workedTime model ++ ")") ]
                        , viewToggleTimer model
                        ]
                    ]
                , viewContentNavigation model
                , viewContent model
                ]


type alias NavigationButton =
    { label : String
    , view : View
    }


viewContentNavigation : Model -> Html Msg
viewContentNavigation model =
    let
        normalStyle =
            "mt-16 mb-8 text-grey-dark mx-2"

        activeStyle =
            "mt-16 mb-8 text-grey-darker mx-2"

        getStyle =
            \btnView ->
                if model.view == btnView then
                    activeStyle

                else
                    normalStyle

        buttons =
            [ { label = "Grid"
              , toView = DailyGrid
              }
            , { label = "List"
              , toView = DailyList
              }
            , { label = "Week"
              , toView = WeekView
              }
            , { label = "Month"
              , toView = MonthView
              }
            ]
                |> List.map
                    (\{ label, toView } ->
                        button [ class (getStyle toView), onClick (ChangedView toView) ]
                            [ span [ class "font-normal" ] [ text label ]
                            ]
                    )
    in
    div [] buttons


viewContent : Model -> Html Msg
viewContent model =
    case model.view of
        DailyGrid ->
            section []
                [ div [ class "container mx-auto" ] [ viewSvg model ]
                ]

        DailyList ->
            section []
                [ div [ class "container mx-auto" ]
                    [ div [ class "flex justify-center" ]
                        [ div []
                            [ table [ class "table" ]
                                [ tbody [] (List.map (\entry -> viewEntry entry model) model.timeEntries |> List.reverse) ]
                            ]
                        ]
                    ]
                ]

        _ ->
            div [] [ text "Not implemented" ]


viewToggleTimer : Model -> Html Msg
viewToggleTimer model =
    let
        style =
            "font-bold rounded-full h-16 w-16 text-2xl -mb-12 shadow-lg text-white"
    in
    case model.timerState of
        Stopped ->
            div [ class "mt-5" ]
                [ button [ onClick StartTimer, class ("bg-green hover:bg-green-dark" ++ style) ]
                    [ span [ class "icon" ] [ i [ class "fas fa-play-circle" ] [] ] ]
                ]

        Running ->
            div [ class "mt-5" ]
                [ button [ onClick StopTimer, class ("bg-red hover:bg-red-dark" ++ style) ]
                    [ span [ class "icon" ] [ i [ class "fas fa-pause-circle" ] [] ] ]
                ]


viewEntry : TimeEntry -> Model -> Html msg
viewEntry entry model =
    let
        duration =
            getEntryDuration entry model.time
                |> toClockTime
                |> viewShortClockTime

        currentTime : ClockTime
        currentTime =
            { hours = toHour utc model.time + myTimeZone
            , minutes = toMinute utc model.time
            , seconds = toSecond utc model.time
            }
    in
    case entry.stop of
        Just stop ->
            tr [ class "border-gray-dark border-0 border-b" ]
                [ td [ class "p-4" ]
                    [ span [ class "text-grey-dark mr-10" ]
                        [ text
                            (viewDateTimeClock entry.start
                                ++ " - "
                                ++ viewDateTimeClock stop
                            )
                        ]
                    , span [] [ text duration ]
                    ]
                ]

        Nothing ->
            tr [ class "border-indigo-darkest border-0 border-b" ]
                [ td [ class "p-4" ]
                    [ span [ class "text-grey-dark mr-10" ]
                        [ text
                            (viewDateTimeClock entry.start
                                ++ " - "
                                ++ viewShortClockTime currentTime
                            )
                        ]
                    , span [] [ text duration ]
                    ]
                ]


viewDateTimeClock : DateTime -> String
viewDateTimeClock dateTime =
    let
        clockTime : ClockTime
        clockTime =
            { hours = dateTime.hours
            , minutes = dateTime.minutes
            , seconds = dateTime.seconds
            }
    in
    viewShortClockTime clockTime


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


getEntryDuration : TimeEntry -> Time.Posix -> Int
getEntryDuration entry time =
    let
        currentTime =
            posixToMillis time // 1000
    in
    if entry.duration > 0 then
        entry.duration

    else
        -- If the time entry is currently running, the duration attribute
        -- contains a negative value, denoting the start of the time entry in
        -- seconds since epoch (Jan 1 1970). The correct duration can be
        -- calculated as current_time + duration, where current_time is the
        -- current time in seconds since epoch.
        --https://github.com/toggl/toggl_api_docs/issues/16#issuecomment-17919490
        currentTime + entry.duration


getWorkedTime : Model -> Int
getWorkedTime model =
    -- Get worked time in seconds
    List.map (\entry -> getEntryDuration entry model.time) model.timeEntries
        |> List.sum


workedTime : Model -> String
workedTime model =
    getWorkedTime model
        |> toClockTime
        |> viewClockTime


todoTime : Model -> String
todoTime model =
    let
        done =
            getWorkedTime model

        -- 7.5 hours
        required =
            7 * 60 * 60 + 1 * 30 * 60

        todo =
            required - done

        overtime =
            todo < 0

        time =
            abs todo
                |> toClockTime
                |> viewClockTime
    in
    if overtime then
        "+ " ++ time

    else
        time


toIntMonth : Month -> Int
toIntMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


getDate : Time.Posix -> String
getDate time =
    let
        year =
            Time.toYear Time.utc time
                |> String.fromInt

        month =
            Time.toMonth Time.utc time
                |> toIntMonth
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay Time.utc time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    year ++ "-" ++ month ++ "-" ++ day


getUrl : String -> String
getUrl date =
    "time_entries?start_date=" ++ date ++ "T00:00:00Z&end_date=" ++ date ++ "T23:59:59Z"


getTimerState : Model -> TimerState
getTimerState model =
    List.reverse model.timeEntries
        |> List.head
        |> (\entry ->
                case entry of
                    Just e ->
                        e.duration

                    Nothing ->
                        0
           )
        |> (\duration ->
                if duration < 0 then
                    Running

                else
                    Stopped
           )



-- HTTP


getTimeEntries : Model -> Cmd Msg
getTimeEntries model =
    Http.request
        { body = Http.emptyBody
        , method = "GET"
        , headers = [ Http.header "Authorization" model.apiAuth ]
        , url = model.apiEndpoint ++ getUrl (getDate model.time)
        , expect = Http.expectJson GotTimeEntries listOfRecordsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


startTimer : Model -> Cmd Msg
startTimer model =
    Http.request
        { body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "time_entry"
                      , Json.Encode.object
                            [ ( "created_with", Json.Encode.string <| "elm" )
                            ]
                      )
                    ]
        , method = "POST"
        , headers = [ Http.header "Authorization" model.apiAuth ]
        , url = model.apiEndpoint ++ "time_entries/start"
        , expect = Http.expectJson StartedTimer singleTimeEntryDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


stopTimer : Model -> Cmd Msg
stopTimer model =
    let
        entry =
            List.reverse model.timeEntries
                |> List.head
    in
    case entry of
        Nothing ->
            Cmd.none

        Just timer ->
            if timer.duration < 0 then
                Http.request
                    { body = Http.emptyBody
                    , method = "PUT"
                    , headers = [ Http.header "Authorization" model.apiAuth ]
                    , url = model.apiEndpoint ++ "time_entries/" ++ String.fromInt timer.id ++ "/stop"
                    , expect = Http.expectJson StoppedTimer singleTimeEntryDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            else
                Cmd.none


type alias TimeEntry =
    { id : Int
    , start : DateTime
    , stop : Maybe DateTime
    , duration : Int
    }


parseToggleDate : String -> DateTime
parseToggleDate dateTimeString =
    let
        -- 2019-04-11T18:35:07+00:00
        dateTime =
            dateTimeString
                |> String.replace "T" " "
                |> String.replace "+" " "
                |> String.split " "
                |> List.take 2
                |> Array.fromList

        date =
            Array.get 0 dateTime
                |> Maybe.withDefault "0000-00-00"
                |> String.split "-"
                |> List.map (\entry -> Maybe.withDefault 0 (String.toInt entry))
                |> Array.fromList

        time =
            Array.get 1 dateTime
                |> Maybe.withDefault "00:00:00"
                |> String.split ":"
                |> List.map (\entry -> Maybe.withDefault 0 (String.toInt entry))
                |> Array.fromList

        result : DateTime
        result =
            { year =
                Array.get 0 date
                    |> Maybe.withDefault 0
            , month =
                Array.get 1 date
                    |> Maybe.withDefault 0
            , day =
                Array.get 2 date
                    |> Maybe.withDefault 0
            , hours =
                modBy 24
                    (myTimeZone
                        + (Array.get 0 time
                            |> Maybe.withDefault 0
                          )
                    )
            , minutes =
                Array.get 1 time
                    |> Maybe.withDefault 0
            , seconds =
                Array.get 2 time
                    |> Maybe.withDefault 0
            }
    in
    result


decodeDateTime : String -> Decoder DateTime
decodeDateTime dateTime =
    let
        parsedDateTime =
            parseToggleDate dateTime
    in
    succeed parsedDateTime


singleTimeEntryDecoder : Decoder TimeEntry
singleTimeEntryDecoder =
    at [ "data" ] timeEntryDecoder


timeEntryDecoder : Decoder TimeEntry
timeEntryDecoder =
    map4
        TimeEntry
        (field "id" int)
        (field "start" string |> andThen decodeDateTime)
        (maybe (field "stop" string |> andThen decodeDateTime))
        (field "duration" int)


listOfRecordsDecoder : Decoder TimeEntries
listOfRecordsDecoder =
    list timeEntryDecoder
