module ElmCanvas exposing (main)

import Browser exposing (sandbox)
import Browser.Events exposing (onMouseDown)
import Html exposing (Html, button, div, text)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Json.Decode exposing (Decoder, field, float, int, map2)
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width, x, y)


main : Program () Model Msg
main =
    Browser.sandbox { init = { coords = [], color = "black", penSize = 3 }, update = update, view = view }


type alias CoordModel =
    { x : Float
    , y : Float
    , color : String
    , size : Int
    }


type alias Model =
    { coords : List CoordModel
    , color : String
    , penSize : Int
    }


type Msg
    = AddPoint Float Float
    | ResetPoint
    | ChangeColor String
    | PlusPenSize
    | MinusPenSize


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPoint x y ->
            { model | coords = { x = x + 10, y = y - 78, color = model.color, size = model.penSize } :: model.coords }

        ResetPoint ->
            { model | coords = [] }

        ChangeColor color ->
            { model | color = color }

        PlusPenSize ->
            { model | penSize = model.penSize + 1 }

        MinusPenSize ->
            { model | penSize = model.penSize - 1 }


onMouseDown function =
    on "mousedown" (map2 function (field "clientX" float) (field "clientY" float))


viewPoint coord =
    circle
        [ cx (String.fromFloat coord.x)
        , cy (String.fromFloat coord.y)
        , r (String.fromInt coord.size)
        , fill coord.color
        , stroke coord.color
        , strokeWidth "0"
        ]
        []


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ Html.h1 [] [ text "「千手観音キャンバス」" ]
            , svg
                [ viewBox "0 0 620 620"
                , width "605"
                , height "605"
                ]
                (List.concat
                    [ [ rect
                            [ x "10"
                            , y "10"
                            , width "600"
                            , height "600"
                            , fill "white"
                            , stroke "black"
                            , strokeWidth "2"
                            , onMouseDown AddPoint
                            ]
                            []
                      ]
                    , List.map (\coord -> viewPoint coord) model.coords
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
