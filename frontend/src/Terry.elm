module Terry exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Decoder, andThen, decodeString, fail, field, int, keyValuePairs, list, maybe, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra
import PersistentState
import String exposing (fromInt)
import Tuple exposing (first, mapSecond, pair, second)
import WebSocket


wsUrl : String
wsUrl =
    "wss://agifq1tuqi.execute-api.eu-west-1.amazonaws.com/dev"


type alias DragId =
    String


type alias DropId =
    String


type alias Score =
    ( String, Int )


type alias Model =
    { country : Maybe String
    , scores : List Score
    , scoreboard : List Score
    , performedCountries : List String
    , allCountries : List ( String, String )
    , error : Maybe String
    , dragDrop : DragDrop.Model DragId DropId
    }


type Msg
    = ReceiveEvent (Result Json.Decode.Error Event)
    | ConnectToBackend String
    | LoadState (Maybe String)
    | DragDropMsg (DragDrop.Msg DragId DropId)


type alias TerryState =
    { country : Maybe String }


type alias Country =
    { event : String
    , country : String
    }


type alias AllCountries =
    { event : String
    , countries : List ( String, String )
    }


type alias PerformedCountries =
    { event : String
    , countries : List String
    }


type alias Scores =
    { event : String
    , scores : List ( String, Int )
    }


type alias InitMessage =
    { action : String
    , country : Maybe String
    }


initMessageEncoder : Model -> Encode.Value
initMessageEncoder model =
    Encode.object
        [ ( "action", Encode.string "init" )
        , ( "country", Json.Encode.Extra.maybe Encode.string model.country )
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


countryLabel : Model -> String -> String
countryLabel model countryCode =
    Maybe.withDefault
        "Somewhere"
        (List.head
            (List.map second (List.filter (\c -> first c == countryCode) model.allCountries))
        )


viewCountry : Model -> Html Msg
viewCountry model =
    case model.country of
        Just country ->
            div [] [ text ("Congratulations, you are representing " ++ countryLabel model country ++ "!") ]

        Nothing ->
            div [] []


viewPerformances : Model -> Html Msg
viewPerformances model =
    div []
        [ h2 [] [ text "Performances" ]
        , ul []
            (List.map
                (\c ->
                    li (DragDrop.draggable DragDropMsg c)
                        [ text (countryLabel model c) ]
                )
                model.performedCountries
            )
        ]


viewScore : Model -> (Score -> List (Attribute Msg)) -> Score -> Html Msg
viewScore model attrs score =
    li (attrs score)
        [ text (countryLabel model (first score) ++ second (mapSecond fromInt score)) ]


viewScores : Model -> Html Msg
viewScores model =
    div []
        [ h2 [] [ text "Scores" ]
        , ul (style "height" "50px" :: DragDrop.droppable DragDropMsg "scores")
            (List.map (viewScore model (\s -> DragDrop.droppable DragDropMsg ("score_" ++ first s))) model.scores)
        ]


viewScoreboard : Model -> Html Msg
viewScoreboard model =
    div []
        [ h2 [] [ text "Scoreboard" ]
        , ul []
            (List.map
                (viewScore model (\_ -> []))
                (List.sortWith (\t1 t2 -> compare (second t2) (second t1)) model.scoreboard)
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
        [ h1 [] [ text "Non-infringing song competition" ]
        , viewCountry model
        , viewPerformances model
        , viewScores model
        , viewScoreboard model
        , viewError model
        ]


updateScores : List Score -> List Score
updateScores scores =
    List.map2 pair
        (List.map first scores)
        (List.append (List.append [ 12, 10 ] (List.reverse (List.range 1 8))) (List.repeat 100 0))


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
                    ( { model | dragDrop = model_, scores = updateScores (( dragId, 0 ) :: model.scores), error = Just dropId }, Cmd.none )

                Nothing ->
                    ( { model | dragDrop = model_ }, Cmd.none )


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
