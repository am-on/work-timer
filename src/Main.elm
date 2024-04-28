port module Main exposing
    ( ApiState(..)
    , Model
    , Msg(..)
    , favicon
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

import Browser
import Derberos.Date.Calendar as DateCalendar
import Derberos.Date.Utils as DateUtils
import Grid exposing (viewGrid)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Iso8601
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , at
        , fail
        , field
        , int
        , map4
        , maybe
        , string
        , succeed
        )
import Json.Encode
import MonthView exposing (viewMonth)
import Task
import Time
import TimeEntry
    exposing
        ( TimeEntries
        , TimeEntry
        , TimerState(..)
        , getEntryDuration
        )
import TimeHelpers
    exposing
        ( toClockTime
        , todoTime
        , viewShortClockTime
        , viewShortPosixTime
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


timerStateString : TimerState -> String
timerStateString state =
    case state of
        Stopped ->
            "Stopped"

        SwitchingToRunning ->
            "SwitchingToRunning"

        Running ->
            "Running"

        SwitchingToStopped _ ->
            "SwitchingToStopped"


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
    , timezone : Time.Zone
    , apiEndpoint : String
    , apiAuth : String
    , apiWorkspaceId : Int
    , view : View
    }


type alias Flags =
    { apiEndpoint : String
    , apiAuth : String
    , apiWorkspaceId : Int
    , time : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model : Model
        model =
            { apiState = Loading
            , timerState = Stopped
            , timeEntries = []
            , monthTimeEntries = []
            , time = Time.millisToPosix flags.time
            , timezone = Time.utc
            , apiEndpoint = flags.apiEndpoint
            , apiAuth = flags.apiAuth
            , apiWorkspaceId = flags.apiWorkspaceId
            , view = DailyGrid
            }
    in
    ( model
    , Cmd.batch
        [ getTimeEntries model
        , Task.perform AdjustTimeZone Time.here
        ]
    )



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
    | AdjustTimeZone Time.Zone
    | ChangedView View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( { model | apiState = Loading, timeEntries = [] }
            , getTimeEntries model
            )

        StartTimer ->
            ( { model | timerState = SwitchingToRunning }, startTimer model )

        StopTimer ->
            ( { model | timerState = SwitchingToStopped model.time }, stopTimer model )

        RefreshAPI _ ->
            ( model
            , getTimeEntries model
            )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | timezone = newZone }
            , Cmd.none
            )

        GotTimeEntries (Ok entries) ->
            let
                newModel =
                    { model
                        | apiState = Success
                        , timeEntries = entries
                        , timerState = getTimerState entries
                    }
            in
            ( newModel
            , favicon (timerStateString newModel.timerState)
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
            , Cmd.batch
                [ getTimeEntries newModel
                , favicon (timerStateString newModel.timerState)
                ]
            )

        StartedTimer (Err _) ->
            ( model, Cmd.none )

        StoppedTimer (Ok _) ->
            let
                newModel =
                    { model | timerState = Stopped }
            in
            ( newModel
            , Cmd.batch
                [ getTimeEntries newModel
                , favicon (timerStateString newModel.timerState)
                ]
            )

        StoppedTimer (Err _) ->
            ( model, Cmd.none )

        ChangedView MonthView ->
            ( { model | view = MonthView, apiState = LoadingContent }
            , getMonthTimeEntries model
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



-- PORTS


port favicon : String -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    case model.apiState of
        Failure ->
            div []
                [ text "I could not get data. "
                , button [ onClick Refresh ] [ text "Try Again!" ]
                , p [] [ text (isoDate model.time model.timezone) ]
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
                [ text (todoTime model.timerState model.timeEntries model.time) ]
            , p [ class "text-white font-normal pb-5" ]
                [ text ("(" ++ viewWorkedTime model.timerState model.timeEntries model.time ++ ")") ]
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

        SwitchingToRunning ->
            div [ class "mt-5" ]
                [ button [ class ("bg-grey-dark cursor-pointer" ++ style) ]
                    [ span [ class "icon" ] [ i [ class "fas fa-play-circle" ] [] ] ]
                ]

        Running ->
            div [ class "mt-5" ]
                [ button [ onClick StopTimer, class ("bg-red hover:bg-red-dark" ++ style) ]
                    [ span [ class "icon" ] [ i [ class "fas fa-pause-circle" ] [] ] ]
                ]

        SwitchingToStopped _ ->
            div [ class "mt-5" ]
                [ button [ class ("bg-grey-dark cursor-pointer" ++ style) ]
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
                [ div [ class "container mx-auto" ] [ viewGrid model.timerState model.timeEntries model.time model.timezone ]
                ]

        DailyList ->
            section []
                [ div [ class "container mx-auto" ]
                    [ div [ class "flex justify-center" ]
                        [ div []
                            [ table [ class "table" ]
                                [ tbody []
                                    (List.map
                                        (\entry -> viewEntry model.timerState entry model.time model.timezone)
                                        model.timeEntries
                                    )
                                ]
                            ]
                        ]
                    ]
                ]

        MonthView ->
            viewMonth model.timerState model.monthTimeEntries model.time

        _ ->
            div [] [ text "Not implemented" ]


viewEntry : TimerState -> TimeEntry -> Time.Posix -> Time.Zone -> Html msg
viewEntry timerState entry time timezone =
    let
        duration =
            getEntryDuration timerState entry time
                |> toClockTime
                |> viewShortClockTime
    in
    case entry.stop of
        Just stop ->
            tr [ class "border-gray-dark border-0 border-b" ]
                [ td [ class "p-4" ]
                    [ span [ class "text-grey-dark mr-10" ]
                        [ text
                            (viewShortPosixTime entry.start timezone
                                ++ " - "
                                ++ viewShortPosixTime stop timezone
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
                            (viewShortPosixTime entry.start timezone
                                ++ " - "
                                ++ viewShortPosixTime time timezone
                            )
                        ]
                    , span [] [ text duration ]
                    ]
                ]


isoDate : Time.Posix -> Time.Zone -> String
isoDate time timezone =
    let
        year =
            Time.toYear timezone time
                |> String.fromInt

        month =
            Time.toMonth timezone time
                |> DateUtils.monthToNumber1
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay timezone time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    year ++ "-" ++ month ++ "-" ++ day


getTimerState : TimeEntries -> TimerState
getTimerState timeEntries =
    timeEntries
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


getMonthTimeEntries : Model -> Cmd Msg
getMonthTimeEntries model =
    let
        todayDate : String
        todayDate =
            isoDate model.time model.timezone

        firstDayOfMonth : String
        firstDayOfMonth =
            isoDate (DateCalendar.getFirstDayOfMonth model.timezone model.time) model.timezone

        url : String
        url =
            model.apiEndpoint
                ++ "me/time_entries?start_date="
                ++ firstDayOfMonth
                ++ "T00:00:00Z&end_date="
                ++ todayDate
                ++ "T23:59:59Z"
    in
    Http.request
        { body = Http.emptyBody
        , method = "GET"
        , headers = [ Http.header "Authorization" model.apiAuth ]
        , url = url
        , expect = Http.expectJson GotMonthTimeEntries listOfRecordsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getTimeEntries : Model -> Cmd Msg
getTimeEntries model =
    let
        todayDate : String
        todayDate =
            isoDate model.time model.timezone

        url : String
        url =
            model.apiEndpoint
                ++ "me/time_entries?start_date="
                ++ todayDate
                ++ "T00:00:00Z&end_date="
                ++ todayDate
                ++ "T23:59:59Z"
    in
    Http.request
        { body = Http.emptyBody
        , method = "GET"
        , headers = [ Http.header "Authorization" model.apiAuth ]
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
                    [ ( "created_with", Json.Encode.string <| "elm" )
                    , ( "duration", Json.Encode.int -1 )
                    , ( "start", Json.Encode.string <| Iso8601.fromTime model.time )
                    , ( "workspace_id", Json.Encode.int model.apiWorkspaceId )
                    ]
        , method = "POST"
        , headers = [ Http.header "Authorization" model.apiAuth ]
        , url =
            model.apiEndpoint
                ++ "workspaces/"
                ++ String.fromInt model.apiWorkspaceId
                ++ "/time_entries"
        , expect = Http.expectJson StartedTimer singleTimeEntryDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


stopTimer : Model -> Cmd Msg
stopTimer model =
    let
        entry =
            model.timeEntries
                |> List.head
    in
    case entry of
        Nothing ->
            Cmd.none

        Just timer ->
            if timer.duration < 0 then
                Http.request
                    { body =
                        Http.jsonBody <|
                            Json.Encode.object
                                [ ( "stop", Json.Encode.string <| Iso8601.fromTime model.time )
                                , ( "workspace_id", Json.Encode.int model.apiWorkspaceId )
                                ]
                    , method = "PUT"
                    , headers = [ Http.header "Authorization" model.apiAuth ]
                    , url =
                        model.apiEndpoint
                            ++ "workspaces/"
                            ++ String.fromInt model.apiWorkspaceId
                            ++ "/time_entries/"
                            ++ String.fromInt timer.id
                    , expect = Http.expectJson StoppedTimer singleTimeEntryDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            else
                Cmd.none


decodeDateTime : String -> Decoder Time.Posix
decodeDateTime dateTime =
    let
        parsedDateTime =
            Iso8601.toTime dateTime
    in
    case parsedDateTime of
        Err _ ->
            fail ("Error parsing " ++ dateTime)

        Ok time ->
            succeed time


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
    Json.Decode.list timeEntryDecoder
