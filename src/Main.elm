module Main exposing
    ( ApiState(..)
    , Model
    , Msg(..)
    , getTimeEntries
    , init
    , listOfRecordsDecoder
    , main
    , subscriptions
    , timeEntryDecoder
    , update
    , view
    , viewEntry
    )

import Array
import Browser
import Derberos.Date.Calendar as DateCalendar
import Derberos.Date.Utils as DateUtils
import Grid exposing (viewGrid)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , at
        , field
        , int
        , list
        , map4
        , maybe
        , string
        , succeed
        )
import Json.Encode
import MonthView exposing (viewMonth)
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
    | LoadingContent
    | Success


type View
    = DailyList
    | DailyGrid
    | WeekView
    | MonthView


type alias Model =
    { apiState : ApiState
    , timerState : TimerState
    , timeEntries : TimeEntries
    , monthTimeEntries : TimeEntries
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { apiState = Loading
            , timerState = Stopped
            , timeEntries = []
            , monthTimeEntries = []
            , time = Time.millisToPosix flags.time
            , apiEndpoint = flags.apiEndpoint
            , apiAuth = flags.apiAuth
            , view = DailyGrid
            }
    in
    ( model, getTimeEntries model.apiEndpoint model.apiAuth model.time model.time )



-- UPDATE


type Msg
    = Refresh
    | StartTimer
    | StopTimer
    | RefreshAPI Time.Posix
    | GotTimeEntries (Result Http.Error TimeEntries)
    | GotMonthTimeEntries (Result Http.Error TimeEntries)
    | StartedTimer (Result Http.Error TimeEntry)
    | StoppedTimer (Result Http.Error TimeEntry)
    | Tick Time.Posix
    | ChangedView View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( { model | apiState = Loading, timeEntries = [] }
            , getTimeEntries model.apiEndpoint model.apiAuth model.time model.time
            )

        StartTimer ->
            ( model, startTimer model )

        StopTimer ->
            ( model, stopTimer model )

        RefreshAPI _ ->
            ( model
            , getTimeEntries model.apiEndpoint model.apiAuth model.time model.time
            )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        GotTimeEntries (Ok entries) ->
            ( { model
                | apiState = Success
                , timeEntries = entries
                , timerState = getTimerState entries
              }
            , Cmd.none
            )

        GotTimeEntries (Err _) ->
            ( { model | apiState = Failure }, Cmd.none )

        GotMonthTimeEntries (Ok entries) ->
            ( { model
                | apiState = Success
                , monthTimeEntries = entries
              }
            , Cmd.none
            )

        GotMonthTimeEntries (Err _) ->
            ( { model | apiState = Failure }, Cmd.none )

        StartedTimer (Ok _) ->
            let
                newModel =
                    { model | timerState = Running }
            in
            ( newModel
            , getTimeEntries newModel.apiEndpoint newModel.apiAuth newModel.time newModel.time
            )

        StartedTimer (Err _) ->
            ( model, Cmd.none )

        StoppedTimer (Ok _) ->
            let
                newModel =
                    { model | timerState = Stopped }
            in
            ( newModel
            , getTimeEntries newModel.apiEndpoint newModel.apiAuth newModel.time newModel.time
            )

        StoppedTimer (Err _) ->
            ( model, Cmd.none )

        ChangedView MonthView ->
            ( { model | view = MonthView, apiState = LoadingContent }
            , getMonthTimeEntries model.apiEndpoint model.apiAuth model.time
            )

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
    case model.apiState of
        Failure ->
            div []
                [ text "I could not get data. "
                , button [ onClick Refresh ] [ text "Try Again!" ]
                , p [] [ text (isoDate model.time) ]
                ]

        Loading ->
            text "Loading..."

        LoadingContent ->
            div []
                [ viewHeader model
                , viewContentNavigation model
                , text "Loading..."
                ]

        Success ->
            div []
                [ viewHeader model
                , viewContentNavigation model
                , viewContent model
                ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "bg-indigo-darker" ]
        [ div [ class "container mx-auto text-center pt-20" ]
            [ h1 [ class "text-white font-normal text-5xl pb-5" ]
                [ text (todoTime model.timeEntries model.time) ]
            , p [ class "text-white font-normal pb-5" ]
                [ text ("(" ++ viewWorkedTime model.timeEntries model.time ++ ")") ]
            , viewTimerToggler model
            ]
        ]


viewTimerToggler : Model -> Html Msg
viewTimerToggler model =
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
                [ div [ class "container mx-auto" ] [ viewGrid model.timeEntries model.time ]
                ]

        DailyList ->
            section []
                [ div [ class "container mx-auto" ]
                    [ div [ class "flex justify-center" ]
                        [ div []
                            [ table [ class "table" ]
                                [ tbody []
                                    (List.map
                                        (\entry -> viewEntry entry model.time)
                                        model.timeEntries
                                        |> List.reverse
                                    )
                                ]
                            ]
                        ]
                    ]
                ]

        MonthView ->
            viewMonth model.monthTimeEntries model.time

        _ ->
            div [] [ text "Not implemented" ]


viewEntry : TimeEntry -> Time.Posix -> Html msg
viewEntry entry time =
    let
        duration =
            getEntryDuration entry time
                |> toClockTime
                |> viewShortClockTime

        currentTime : ClockTime
        currentTime =
            { hours = Time.toHour Time.utc time + myTimeZone
            , minutes = Time.toMinute Time.utc time
            , seconds = Time.toSecond Time.utc time
            }
    in
    case entry.stop of
        Just stop ->
            tr [ class "border-gray-dark border-0 border-b" ]
                [ td [ class "p-4" ]
                    [ span [ class "text-grey-dark mr-10" ]
                        [ text
                            (viewShortClockTime entry.start
                                ++ " - "
                                ++ viewShortClockTime stop
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
                            (viewShortClockTime entry.start
                                ++ " - "
                                ++ viewShortClockTime currentTime
                            )
                        ]
                    , span [] [ text duration ]
                    ]
                ]


isoDate : Time.Posix -> String
isoDate time =
    let
        year =
            Time.toYear Time.utc time
                |> String.fromInt

        month =
            Time.toMonth Time.utc time
                |> DateUtils.monthToNumber1
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay Time.utc time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    year ++ "-" ++ month ++ "-" ++ day


getTimerState : TimeEntries -> TimerState
getTimerState timeEntries =
    timeEntries
        |> List.reverse
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


getMonthTimeEntries : String -> String -> Time.Posix -> Cmd Msg
getMonthTimeEntries apiEndpoint apiAuth time =
    let
        url =
            apiEndpoint
                ++ "time_entries?start_date="
                ++ isoDate (DateCalendar.getFirstDayOfMonth Time.utc time)
                ++ "T00:00:00Z&end_date="
                ++ isoDate time
                ++ "T23:59:59Z"
    in
    Http.request
        { body = Http.emptyBody
        , method = "GET"
        , headers = [ Http.header "Authorization" apiAuth ]
        , url = url
        , expect = Http.expectJson GotMonthTimeEntries listOfRecordsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getTimeEntries : String -> String -> Time.Posix -> Time.Posix -> Cmd Msg
getTimeEntries apiEndpoint apiAuth start end =
    let
        url =
            apiEndpoint
                ++ "time_entries?start_date="
                ++ isoDate start
                ++ "T00:00:00Z&end_date="
                ++ isoDate end
                ++ "T23:59:59Z"
    in
    Http.request
        { body = Http.emptyBody
        , method = "GET"
        , headers = [ Http.header "Authorization" apiAuth ]
        , url = url
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
                    , url =
                        model.apiEndpoint
                            ++ "time_entries/"
                            ++ String.fromInt timer.id
                            ++ "/stop"
                    , expect = Http.expectJson StoppedTimer singleTimeEntryDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            else
                Cmd.none


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
