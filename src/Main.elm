module Main exposing (main)

import Browser exposing (sandbox)
import Browser.Events exposing (onMouseMove)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onMouseDown, onMouseOut, onMouseOver)
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width, x, y)


main : Program () Model Msg
main =
    sandbox { init = { coords = [ { x = 0.0, y = 0.0 } ], color = "black", penSize = 3 }, update = update, view = view }


type alias CoordModel =
    { x : Float
    , y : Float
    }


type alias Model =
    { coords : List CoordModel
    , color : String
    , penSize : Int
    }


type Msg
    = AddPoint
    | ResetPoint
    | ChangeColor String
    | PlusPenSize
    | MinusPenSize


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPoint ->
            case List.head model.coords of
                Just coord ->
                    { model | coords = [ { x = coord.x + 10.0, y = coord.y + 10.0 } ] ++ model.coords }

                Nothing ->
                    model

        ResetPoint ->
            { model | coords = [ { x = 0.0, y = 0.0 } ] }

        ChangeColor color ->
            { model | color = color }

        PlusPenSize ->
            { model | penSize = model.penSize + 1 }

        MinusPenSize ->
            { model | penSize = model.penSize - 1 }


viewPoint coord color penSize =
    circle
        [ cx (String.fromFloat coord.x)
        , cy (String.fromFloat coord.y)
        , r (String.fromInt penSize)
        , fill color
        , stroke color
        , strokeWidth "0"
        ]
        []


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ div [] [ text "ちょっぴり不思議なお絵描きキャンバス" ]
            , svg
                [ viewBox "0 0 770 770"
                , width "750"
                , height "750"
                ]
                (List.concat
                    [ [ rect
                            [ x "10"
                            , y "10"
                            , width "750"
                            , height "750"
                            , fill "white"
                            , stroke "black"
                            , strokeWidth "2"
                            , onClick AddPoint
                            ]
                            []
                      ]
                    , List.map (\coord -> viewPoint coord model.color model.penSize) model.coords
                    ]
                )
            , div
                []
                [ button
                    [ onClick ResetPoint ]
                    [ text "RESET" ]
                ]
            , div
                []
                [ button
                    [ onClick (ChangeColor "black") ]
                    [ text "BLACK" ]
                , button
                    [ onClick (ChangeColor "red") ]
                    [ text "RED" ]
                , button
                    [ onClick (ChangeColor "blue") ]
                    [ text "BLUE" ]
                , button
                    [ onClick (ChangeColor "green") ]
                    [ text "GREEN" ]
                ]
            , div
                []
                [ button
                    [ onClick MinusPenSize ]
                    [ text "-" ]
                , button
                    [ onClick PlusPenSize ]
                    [ text "+" ]
                ]
            ]
        ]
