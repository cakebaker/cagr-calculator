module Main exposing (..)

import Html exposing (Html, label, input, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { initialValue : Float
    , endValue : Float
    , timePeriod : Float
    }


model : Model
model =
    { initialValue = invalidValue
    , endValue = invalidValue
    , timePeriod = invalidValue
    }


invalidValue : Float
invalidValue =
    -1


type Msg
    = InitialValue String
    | EndValue String
    | TimePeriod String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InitialValue newValue ->
            { model
                | initialValue =
                    case String.toFloat (newValue) of
                        Result.Ok value ->
                            if value > 0 then
                                value
                            else
                                invalidValue

                        _ ->
                            invalidValue
            }

        EndValue newValue ->
            { model
                | endValue = Result.withDefault invalidValue (String.toFloat (newValue))
            }

        TimePeriod newValue ->
            { model
                | timePeriod =
                    case String.toFloat (newValue) of
                        Result.Ok value ->
                            if value > 0 then
                                value
                            else
                                invalidValue

                        _ ->
                            invalidValue
            }


view : Model -> Html Msg
view model =
    div [ id "cagr" ]
        [ div []
            [ label [] [ text "Initial value" ]
            , input [ autofocus True, onInput InitialValue ] []
            ]
        , div []
            [ label [] [ text "End value" ]
            , input [ onInput EndValue ] []
            ]
        , div []
            [ label [] [ text "Time period (in years)" ]
            , input [ onInput TimePeriod ] []
            ]
        , div []
            [ label [] [ text "CAGR" ]
            , input
                [ id "result"
                , readonly True
                , value
                    (case calculateCagr model of
                        Result.Ok value ->
                            toPercentageString value

                        _ ->
                            ""
                    )
                ]
                []
            ]
        ]


toPercentageString : Float -> String
toPercentageString value =
    let
        percentValue =
            100 * value

        -- XXX
        toTwoDecimalPlaces : Float -> Float
        toTwoDecimalPlaces x =
            let
                rounded =
                    round (100 * x)
            in
                (toFloat rounded) / 100
    in
        (toString (toTwoDecimalPlaces percentValue)) ++ "%"


calculateCagr : Model -> Result String Float
calculateCagr model =
    if (model.initialValue == invalidValue) || (model.endValue == invalidValue) || (model.timePeriod == invalidValue) then
        Result.Err "Error"
    else
        Result.Ok ((model.endValue / model.initialValue) ^ (1 / model.timePeriod) - 1)
