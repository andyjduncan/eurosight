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
    , scoreboard : List Score
    , performedCountries : List CountryId
    , allCountries : List ( CountryId, String )
    , error : Maybe String
    , dragDrop : DragDrop.Model DragId DropId
    }


type VoteBlock
    = First
    | Ten
    | Twelve
    | AllVotes


type Msg
    = ReceiveEvent (Result Json.Decode.Error Event)
    | ConnectToBackend String
    | LoadState (Maybe String)
    | DragDropMsg (DragDrop.Msg DragId DropId)
    | SubmitVotes VoteBlock


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


type alias Scores =
    { event : String
    , scores : List ( CountryId, Int )
    }


type alias InitMessage =
    { action : String
    , country : Maybe CountryId
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


type Event
    = CountryEvent Country
    | AllCountriesEvent AllCountries
    | PerformedCountriesEvent PerformedCountries
    | ScoresEvent Scores


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


scoresDecoder : Decoder Scores
scoresDecoder =
    succeed Scores
        |> required "event" string
        |> required "scores" (keyValuePairs int)


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

                "scores" ->
                    Json.Decode.map ScoresEvent scoresDecoder

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
    , scoreboard = []
    , performedCountries = []
    , allCountries = []
    , error = Nothing
    , dragDrop = DragDrop.init
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, PersistentState.loadState () )


viewTitle : Model -> Html Msg
viewTitle model =
    div [ class "container" ]
        [ h1 [] [ text "Non-infringing song competition" ]
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
        DragDrop.draggable
            DragDropMsg
            (DragPerformed country)

    else
        [ class "disabled" ]


viewPerformances : Model -> Html Msg
viewPerformances model =
    div [ class "container" ]
        [ h2 [] [ text "Performances" ]
        , div [ class "list-unstyled", class "card-columns" ]
            (List.map
                (\c ->
                    button ([ class "list-group-item", class "list-group-item-action" ] ++ performanceAttr model c)
                        [ text (countryLabel model c) ]
                )
                model.performedCountries
            )
        ]


viewScore : Model -> (Score -> List (Attribute Msg)) -> Score -> Html Msg
viewScore model attrs score =
    li ([ class "list-group-item", class "list-group-item-action", class "d-flex", class "justify-content-between", class "align-items-center" ] ++ attrs score)
        [ text
            (countryLabel model (first score))
        , span [ class "badge", class "badge-primary" ]
            [ text (second (mapSecond fromInt score)) ]
        ]


viewScores : Model -> Html Msg
viewScores model =
    div [ class "container" ]
        [ h2 [] [ text "Scores" ]
        , ul (class "list-group" :: style "min-height" "100px" :: DragDrop.droppable DragDropMsg DropScoreboard)
            (List.map
                (viewScore model
                    (\s ->
                        DragDrop.droppable DragDropMsg (DropScored (first s))
                            ++ DragDrop.draggable DragDropMsg (DragScored (first s))
                    )
                )
                model.scores
            )
        , div
            []
            [ button
                [ onClick (SubmitVotes AllVotes)
                , disabled (length model.scores < 10)
                ]
                [ text "Submit Votes" ]
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
                model.scoreboard
            )
        ]


viewError : Model -> Html Msg
viewError model =
    case model.error of
        Just a ->
            div []
                [ text a ]

        Nothing ->
            div [] []


view : Model -> Html Msg
view model =
    div []
        [ viewTitle model
        , viewCountry model
        , viewScoreboard model
        , viewScores model
        , viewPerformances model
        , viewError model
        ]


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
                First ->
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

                ScoresEvent scores ->
                    ( { model | scoreboard = scores.scores, error = Nothing }, Cmd.none )

        ReceiveEvent (Err error) ->
            Debug.log (toString error)
                ( { model | error = Just (toString error) }, Cmd.none )

        ConnectToBackend message ->
            ( { model | error = Just message }, WebSocket.sendMessage (Encode.encode 0 (initMessageEncoder model)) )

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
            ( model, WebSocket.sendMessage (Encode.encode 0 (voteMessageEncoder (filterScoresForVote block model.scores))) )


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
