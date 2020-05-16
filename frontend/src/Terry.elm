module Terry exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Decoder, andThen, decodeString, fail, field, int, keyValuePairs, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra
import List exposing (filter, length)
import List.Extra exposing (filterNot, splitWhen, zip)
import PersistentState
import String exposing (fromInt)
import Tuple exposing (first, mapSecond, second)
import WebSocket


wsUrl : String
wsUrl =
    "wss://agifq1tuqi.execute-api.eu-west-1.amazonaws.com/dev"


type DragId
    = DragPerformed CountryId
    | DragScored CountryId


type DropId
    = DropScoreboard
    | DropScored CountryId


type alias CountryId =
    String


type alias Score =
    ( CountryId, Int )


type alias Model =
    { country : Maybe CountryId
    , scores : List Score
    , votingDisabled : Bool
    , votesFirstReceived : Bool
    , votesTenReceived : Bool
    , votesTwelveReceived : Bool
    , scoreboard : List Score
    , performedCountries : List CountryId
    , performingCountries : List CountryId
    , votingPanels : List CountryId
    , allCountries : List ( CountryId, String )
    , error : Maybe String
    , dragDrop : DragDrop.Model DragId DropId
    , isAdmin : Bool
    }


type VoteBlock
    = FirstVotes
    | Ten
    | Twelve
    | AllVotes


type Direction
    = Up
    | Down


type Msg
    = ReceiveEvent (Result Json.Decode.Error Event)
    | ConnectToBackend String
    | LoadState (Maybe String)
    | DragDropMsg (DragDrop.Msg DragId DropId)
    | SelectPerformer CountryId
    | MoveScore Direction CountryId
    | SubmitVotes VoteBlock
    | SubmitPerformance CountryId
    | SubmitEnableVoting CountryId
    | SubmitRefresh


type alias TerryState =
    { country : Maybe String }


type alias Country =
    { event : String
    , country : CountryId
    }


type alias AllCountries =
    { event : String
    , countries : List ( CountryId, String )
    }


type alias PerformedCountries =
    { event : String
    , countries : List CountryId
    }


type alias PerformingCountries =
    { event : String
    , countries : List CountryId
    }


type alias VotingPanels =
    { event : String
    , countries : List CountryId
    }


type alias Scores =
    { event : String
    , scores : List ( CountryId, Int )
    }


type alias InitMessage =
    { action : String
    , country : Maybe CountryId
    }


type alias VotingEnabledMessage =
    { action : String }


type alias AdminMessage =
    { action : String
    }


initMessageEncoder : Model -> Encode.Value
initMessageEncoder model =
    Encode.object
        [ ( "action", Encode.string "init" )
        , ( "country", Json.Encode.Extra.maybe Encode.string model.country )
        ]


scoreEncoder : Score -> Encode.Value
scoreEncoder score =
    Encode.object
        [ ( first score, Encode.int (second score) ) ]


voteMessageEncoder : List Score -> Encode.Value
voteMessageEncoder scores =
    Encode.object
        [ ( "action", Encode.string "vote" )
        , ( "scores", Encode.list scoreEncoder scores )
        ]


performanceMessageEncoder : CountryId -> Encode.Value
performanceMessageEncoder countryId =
    Encode.object
        [ ( "action", Encode.string "countryPerformance" )
        , ( "country", Encode.string countryId )
        ]


enableVotingMessageEncoder : CountryId -> Encode.Value
enableVotingMessageEncoder countryId =
    Encode.object
        [ ( "action", Encode.string "enableVoting" )
        , ( "country", Encode.string countryId )
        ]


refreshMessageEncoder : Encode.Value
refreshMessageEncoder =
    Encode.object
        [ ( "action", Encode.string "refresh" )
        ]


type Event
    = CountryEvent Country
    | AllCountriesEvent AllCountries
    | PerformedCountriesEvent PerformedCountries
    | PerformingCountriesEvent PerformingCountries
    | VotingPanelsEvent VotingPanels
    | ScoresEvent Scores
    | VotingEnabledEvent VotingEnabledMessage
    | AdminEvent AdminMessage


countryDecoder : Decoder Country
countryDecoder =
    succeed Country
        |> required "event" string
        |> required "country" string


allCountriesDecoder : Decoder AllCountries
allCountriesDecoder =
    succeed AllCountries
        |> required "event" string
        |> required "countries" (keyValuePairs string)


performedCountriesDecoder : Decoder PerformedCountries
performedCountriesDecoder =
    succeed PerformedCountries
        |> required "event" string
        |> required "countries" (list string)


performingCountriesDecoder : Decoder PerformingCountries
performingCountriesDecoder =
    succeed PerformingCountries
        |> required "event" string
        |> required "countries" (list string)


votingPanelsDecoder : Decoder VotingPanels
votingPanelsDecoder =
    succeed VotingPanels
        |> required "event" string
        |> required "countries" (list string)


scoresDecoder : Decoder Scores
scoresDecoder =
    succeed Scores
        |> required "event" string
        |> required "scores" (keyValuePairs int)


votingEnabledDecoder : Decoder AdminMessage
votingEnabledDecoder =
    succeed AdminMessage
        |> required "event" string


adminDecoder : Decoder AdminMessage
adminDecoder =
    succeed AdminMessage
        |> required "event" string


eventDecoder : Decoder Event
eventDecoder =
    let
        toEvent eventType =
            case eventType of
                "country" ->
                    Json.Decode.map CountryEvent countryDecoder

                "allCountries" ->
                    Json.Decode.map AllCountriesEvent allCountriesDecoder

                "performedCountries" ->
                    Json.Decode.map PerformedCountriesEvent performedCountriesDecoder

                "performingCountries" ->
                    Json.Decode.map PerformingCountriesEvent performingCountriesDecoder

                "votingPanels" ->
                    Json.Decode.map VotingPanelsEvent votingPanelsDecoder

                "scores" ->
                    Json.Decode.map ScoresEvent scoresDecoder

                "votingEnabled" ->
                    Json.Decode.map VotingEnabledEvent votingEnabledDecoder

                "madeAdmin" ->
                    Json.Decode.map AdminEvent adminDecoder

                _ ->
                    fail ("Unknown event " ++ eventType)
    in
    field "event" string
        |> andThen toEvent


stateDecoder : Decoder TerryState
stateDecoder =
    succeed TerryState
        |> optional "country" (Json.Decode.map Just string) Nothing


stateEncoder : TerryState -> Encode.Value
stateEncoder state =
    Encode.object
        [ ( "country", Json.Encode.Extra.maybe Encode.string state.country ) ]


initialModel : Model
initialModel =
    { country = Nothing
    , scores = []
    , votingDisabled = True
    , votesFirstReceived = False
    , votesTenReceived = False
    , votesTwelveReceived = False
    , scoreboard = []
    , performedCountries = []
    , performingCountries = []
    , votingPanels = []
    , allCountries = []
    , error = Nothing
    , dragDrop = DragDrop.init
    , isAdmin = False
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, PersistentState.loadState () )


viewTitle : Model -> Html Msg
viewTitle model =
    div [ class "container" ]
        [ h1 [] [ text "Quarantine Song Competition" ]
        ]


countryLabel : Model -> String -> String
countryLabel model countryCode =
    Maybe.withDefault
        "Somewhere"
        (List.head
            (List.map second (List.filter (\c -> first c == countryCode) model.allCountries))
        )


viewCountry : Model -> Html Msg
viewCountry model =
    let
        countryText =
            case model.country of
                Just country ->
                    "Congratulations, you are voting on behalf of " ++ countryLabel model country ++ "!"

                Nothing ->
                    "Looking for countries without voting panels..."
    in
    div [ class "container" ]
        [ h2 [] [ text countryText ]
        ]


performanceAttr : Model -> String -> List (Attribute Msg)
performanceAttr model country =
    if not (List.any (\s -> country == first s) model.scores) then
        onClick (SelectPerformer country)
            :: DragDrop.draggable
                DragDropMsg
                (DragPerformed country)

    else
        [ class "disabled" ]


viewPerformances : Model -> Html Msg
viewPerformances model =
    div [ class "container" ]
        [ h2 [] [ text "Performances" ]
        , div [] [ text "Click on or drag countries up to give them points" ]
        , div [ class "list-unstyled", class "card-columns" ]
            (List.map
                (\c ->
                    button ([ class "list-group-item", class "list-group-item-action" ] ++ performanceAttr model c)
                        [ text (countryLabel model c) ]
                )
                model.performedCountries
            )
        ]


votingDisabled : Model -> VoteBlock -> Bool
votingDisabled model voteBlock =
    let
        voteBlockReceived =
            case voteBlock of
                FirstVotes ->
                    model.votesFirstReceived

                Ten ->
                    model.votesTenReceived

                Twelve ->
                    model.votesTwelveReceived

                AllVotes ->
                    True
    in
    model.votingDisabled
        || voteBlockReceived
        || (length model.scores
                < 10
           )


viewScores : Model -> Html Msg
viewScores model =
    div [ class "container" ]
        [ h2 [] [ text "Scores" ]
        , div [] [ text "Move countries up from the performance list to give them points.  Click on the arrows or drag them to re-order.  Points are awarded from 12 down to 1" ]
        , ul (class "list-group" :: class "alert" :: class "alert-info" :: style "min-height" "100px" :: DragDrop.droppable DragDropMsg DropScoreboard)
            (List.map
                (\s ->
                    li
                        ([ class "list-group-item"
                         , class "list-group-item-action"
                         , class "d-flex"
                         , class "justify-content-between"
                         , class "align-items-center"
                         ]
                            ++ DragDrop.droppable DragDropMsg (DropScored (first s))
                            ++ DragDrop.draggable DragDropMsg (DragScored (first s))
                        )
                        [ text
                            (countryLabel model (first s))
                        , span []
                            [ div [ class "btn-group", style "padding-right" "10px" ]
                                [ button [ class "btn", class "btn-secondary", onClick (MoveScore Up (first s)) ] [ text "⇧" ]
                                , button [ class "btn", class "btn-secondary", onClick (MoveScore Down (first s)) ] [ text "⇩" ]
                                ]
                            , span [ class "badge", class "badge-primary" ]
                                [ text (second (mapSecond fromInt s)) ]
                            ]
                        ]
                )
                model.scores
            )
        , div
            []
            [ button
                [ class "btn"
                , class
                    (if votingDisabled model FirstVotes then
                        "btn-light"

                     else
                        "btn-success"
                    )
                , onClick
                    (SubmitVotes FirstVotes)
                , disabled (votingDisabled model FirstVotes)
                ]
                [ text "Submit initial points" ]
            , button
                [ class "btn"
                , class
                    (if votingDisabled model Ten then
                        "btn-light"

                     else
                        "btn-success"
                    )
                , onClick
                    (SubmitVotes Ten)
                , disabled (votingDisabled model Ten)
                ]
                [ text "Submit 10 points" ]
            , button
                [ class "btn"
                , class
                    (if votingDisabled model Twelve then
                        "btn-light"

                     else
                        "btn-success"
                    )
                , onClick
                    (SubmitVotes Twelve)
                , disabled (votingDisabled model Twelve)
                ]
                [ text "Submit 12 points" ]
            ]
        ]


viewScoreboard : Model -> Html Msg
viewScoreboard model =
    div [ class "container" ]
        [ h2 [] [ text "Scoreboard" ]
        , div [ class "list-unstyled", class "card-columns" ]
            (List.map
                (\c ->
                    div [ class "list-group-item", class "d-flex", class "justify-content-between", class "align-items-center" ]
                        [ text
                            (countryLabel model (first c))
                        , span [ class "badge", class "badge-primary" ]
                            [ text (second (mapSecond fromInt c)) ]
                        ]
                )
                (List.sortWith (\t1 t2 -> compare (second t2) (second t1)) model.scoreboard)
            )
        ]


viewError : Model -> Html Msg
viewError model =
    div [ class "container" ]
        [ case model.error of
            Just a ->
                div []
                    [ text a ]

            Nothing ->
                div [] []
        , div
            []
            [ button
                [ class "btn"
                , onClick SubmitRefresh
                ]
                [ text "Refresh" ]
            ]
        ]


performingAttr : Model -> CountryId -> List (Attribute Msg)
performingAttr model country =
    if List.member country model.performedCountries then
        [ class "list-group-item-success", class "disabled", disabled True ]

    else
        [ class "list-group-item-danger", onClick (SubmitPerformance country) ]


viewAdmin : Model -> Html Msg
viewAdmin model =
    div [ class "container" ]
        [ h2 [] [ text "Admin Tools" ]
        , h3 [] [ text "Performing Countries" ]
        , div [ class "list-unstyled", class "card-columns" ]
            (List.map
                (\c ->
                    button
                        ([ class "list-group-item"
                         , class "list-group-item-action"

                         --, onClick SelectForScoring c
                         ]
                            ++ performingAttr model c
                        )
                        [ text (countryLabel model c) ]
                )
                model.performingCountries
            )
        , h3 [] [ text "Voting Panels" ]
        , div [ class "list-unstyled", class "card-columns" ]
            (List.map
                (\c ->
                    button [ class "list-group-item", class "list-group-item-action", onClick (SubmitEnableVoting c) ]
                        [ text (countryLabel model c) ]
                )
                model.votingPanels
            )
        ]


view : Model -> Html Msg
view model =
    div []
        ([ viewTitle model
         , viewScoreboard model
         , viewCountry model
         , viewScores model
         , viewPerformances model
         , viewError model
         ]
            ++ (if model.isAdmin then
                    [ viewAdmin model ]

                else
                    []
               )
        )


updateScores : List Score -> List Score
updateScores scores =
    zip
        (List.map first scores)
        ([ 12, 10 ] ++ List.reverse (List.range 1 8))


moveScore : DragId -> DropId -> List Score -> List Score
moveScore dragId dropId currentScores =
    let
        draggedCountry =
            case dragId of
                DragPerformed country ->
                    country

                DragScored country ->
                    country
    in
    case dropId of
        DropScoreboard ->
            ( draggedCountry, 0 ) :: filterNot (\s -> draggedCountry == first s) currentScores

        DropScored target ->
            case splitWhen (\s -> target == first s) currentScores of
                Just ( before, after ) ->
                    filterNot (\s -> draggedCountry == first s) before
                        ++ (( draggedCountry, 0 ) :: filterNot (\s -> draggedCountry == first s) after)

                Nothing ->
                    ( draggedCountry, 0 ) :: currentScores


filterScoresForVote : VoteBlock -> List Score -> List Score
filterScoresForVote voteBlock scores =
    let
        scoresFilter =
            case voteBlock of
                FirstVotes ->
                    \s -> second s < 10

                Ten ->
                    \s -> second s == 10

                Twelve ->
                    \s -> second s == 12

                AllVotes ->
                    \_ -> True
    in
    filter scoresFilter scores


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveEvent (Ok event) ->
            case event of
                CountryEvent countryEvent ->
                    ( { model | country = Just countryEvent.country, error = Nothing }
                    , PersistentState.saveState (Encode.encode 0 (stateEncoder { country = Just countryEvent.country }))
                    )

                AllCountriesEvent allCountries ->
                    ( { model | allCountries = allCountries.countries, error = Nothing }, Cmd.none )

                PerformedCountriesEvent performedCountries ->
                    ( { model | performedCountries = performedCountries.countries, error = Nothing }, Cmd.none )

                PerformingCountriesEvent performingCountries ->
                    ( { model | performingCountries = performingCountries.countries, error = Nothing }, Cmd.none )

                VotingPanelsEvent votingPanels ->
                    ( { model | votingPanels = votingPanels.countries, error = Nothing }, Cmd.none )

                ScoresEvent scores ->
                    ( { model | scoreboard = scores.scores, error = Nothing }, Cmd.none )

                VotingEnabledEvent _ ->
                    ( { model | votingDisabled = False, error = Nothing }, Cmd.none )

                AdminEvent _ ->
                    ( { model | isAdmin = True }, Cmd.none )

        ReceiveEvent (Err error) ->
            Debug.log (toString error)
                ( { model | error = Just (toString error) }, Cmd.none )

        ConnectToBackend _ ->
            ( model, WebSocket.sendMessage (Encode.encode 0 (initMessageEncoder model)) )

        LoadState (Just stateString) ->
            let
                decodedState =
                    decodeString stateDecoder stateString
            in
            case decodedState of
                Ok newState ->
                    ( { model | country = newState.country }, WebSocket.listen wsUrl )

                Err error ->
                    ( { model | error = Just (toString error) }, Cmd.none )

        LoadState Nothing ->
            ( model, WebSocket.listen wsUrl )

        SelectPerformer country ->
            ( { model
                | scores = updateScores (moveScore (DragPerformed country) DropScoreboard model.scores)
                , error = Nothing
              }
            , Cmd.none
            )

        MoveScore direction country ->
            let
                maybeTarget =
                    case direction of
                        Up ->
                            model.scores
                                |> zip (List.drop 1 model.scores)
                                |> List.filter (\( current, next ) -> first current == country)
                                |> List.map (\( current, next ) -> first next)
                                |> List.head

                        Down ->
                            List.reverse model.scores
                                |> zip (List.drop 2 (List.reverse model.scores))
                                |> List.filter (\( prev, current ) -> first prev == country)
                                |> List.map (\( prev, current ) -> first current)
                                |> List.head
            in
            case maybeTarget of
                Just dropTarget ->
                    ( { model
                        | scores = updateScores (moveScore (DragPerformed country) (DropScored dropTarget) model.scores)
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            case result of
                Just ( dragId, dropId, _ ) ->
                    ( { model
                        | dragDrop = model_
                        , scores = updateScores (moveScore dragId dropId model.scores)
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | dragDrop = model_ }, Cmd.none )

        SubmitVotes block ->
            case block of
                FirstVotes ->
                    ( { model | votesFirstReceived = True }
                    , WebSocket.sendMessage (Encode.encode 0 (voteMessageEncoder (filterScoresForVote block model.scores)))
                    )

                Ten ->
                    ( { model | votesTenReceived = True }
                    , WebSocket.sendMessage (Encode.encode 0 (voteMessageEncoder (filterScoresForVote block model.scores)))
                    )

                Twelve ->
                    ( { model | votesTwelveReceived = True }
                    , WebSocket.sendMessage (Encode.encode 0 (voteMessageEncoder (filterScoresForVote block model.scores)))
                    )

                AllVotes ->
                    ( { model | votesFirstReceived = True, votesTenReceived = True, votesTwelveReceived = True }
                    , WebSocket.sendMessage (Encode.encode 0 (voteMessageEncoder (filterScoresForVote block model.scores)))
                    )

        SubmitPerformance country ->
            ( model, WebSocket.sendMessage (Encode.encode 0 (performanceMessageEncoder country)) )

        SubmitEnableVoting country ->
            ( model, WebSocket.sendMessage (Encode.encode 0 (enableVotingMessageEncoder country)) )

        SubmitRefresh ->
            ( model, WebSocket.sendMessage (Encode.encode 0 refreshMessageEncoder) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.receive (ReceiveEvent << decodeString eventDecoder)
        , WebSocket.connect ConnectToBackend
        , PersistentState.onLoadState LoadState
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
