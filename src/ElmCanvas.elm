module ElmCanvas exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width, x, y)


main : Program () Model Msg
main =
    Browser.sandbox { init = { points = [], color = "black", penSize = 3, penNum = 5, isClick = False }, update = update, view = view }


type alias CoordModel =
    { x : Float
    , y : Float
    }


type alias PointModel =
    { coord : CoordModel
    , color : String
    , size : Int
    }


type alias Model =
    { points : List PointModel
    , color : String
    , penSize : Int
    , penNum : Int
    , isClick : Bool
    }


type Msg
    = AddPoint Float Float
    | ResetPoint
    | ChangeColor String
    | PlusPenSize
    | MinusPenSize
    | PlusPenNum
    | MinusPenNum
    | EnablePen
    | DisablePen


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPoint x y ->
            if model.isClick then
                { model | points = model.points ++ addCoords model.penNum model.penNum (x + 10) (y - 78) model.color model.penSize }

            else
                model

        ResetPoint ->
            { model | points = [] }

        ChangeColor color ->
            { model | color = color }

        PlusPenSize ->
            { model | penSize = model.penSize + 1 }

        MinusPenSize ->
            if model.penSize > 1 then
                { model | penSize = model.penSize - 1 }

            else
                model

        PlusPenNum ->
            { model | penNum = model.penNum + 1 }

        MinusPenNum ->
            if model.penNum > 1 then
                { model | penNum = model.penNum - 1 }

            else
                model

        EnablePen ->
            { model | isClick = True }

        DisablePen ->
            { model | isClick = False }


onMouseMove function =
    on "mousemove" (map2 function (field "clientX" float) (field "clientY" float))


addCoords : Int -> Int -> Float -> Float -> String -> Int -> List PointModel
addCoords penNum remainingLineCnt x y color size =
    if remainingLineCnt == 0 then
        [ { coord = { x = x, y = y }, color = color, size = size } ]

    else
        { coord = rotateCoord x y penNum, color = color, size = size } :: addCoords penNum (remainingLineCnt - 1) (rotateCoord x y penNum).x (rotateCoord x y penNum).y color size


rotateCoord x y penNum =
    let
        rad =
            2 * pi / toFloat penNum

        rotate_x =
            x * cos rad - y * sin rad + 300 - 300 * cos rad + 300 * sin rad

        rotate_y =
            x * sin rad + y * cos rad + 300 - 300 * sin rad - 300 * cos rad
    in
    { x = rotate_x, y = rotate_y }


viewPoint point =
    if point.coord.x > 10 && point.coord.x < 610 && point.coord.y > 10 && point.coord.y < 610 then
        circle
            [ cx (String.fromFloat point.coord.x)
            , cy (String.fromFloat point.coord.y)
            , r (String.fromInt point.size)
            , fill point.color
            , stroke point.color
            , strokeWidth "0"
            ]
            []

    else
        circle
            [ cx (String.fromFloat point.coord.x)
            , cy (String.fromFloat point.coord.y)
            , r "0"
            , fill point.color
            , stroke point.color
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
                , style "width" "605"
                , style "height" "605"
                ]
                (List.concat
                    [ [ rect
                            [ x "10"
                            , y "10"
                            , style "width" "600"
                            , style "height" "600"
                            , fill "white"
                            , stroke "black"
                            , strokeWidth "2"
                            , onMouseMove AddPoint
                            , onMouseDown EnablePen
                            , onMouseUp DisablePen
                            ]
                            []
                      ]
                    , List.map (\coord -> viewPoint coord) model.points
                    ]
                )
            , div
                []
                [ text "penNum "
                , button
                    [ onClick MinusPenNum ]
                    [ text "-" ]
                , text (" " ++ String.fromInt model.penNum ++ " ")
                , button
                    [ onClick PlusPenNum ]
                    [ text "+" ]
                ]
            , div
                []
                [ text "penSize "
                , button
                    [ onClick MinusPenSize ]
                    [ text "-" ]
                , text (" " ++ String.fromInt model.penSize ++ " ")
                , button
                    [ onClick PlusPenSize ]
                    [ text "+" ]
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
                    [ onClick ResetPoint ]
                    [ text "RESET" ]
                ]
            ]
        ]
