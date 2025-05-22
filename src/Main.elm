module Main exposing (main)

import Browser

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

main = Browser.sandbox
    { init = initialModel
    , view = view
    , update = update }

type State
    = Nothing
    | X
    | O

type Msg -- this is so ugly
    = ToggleS1
    | ToggleS2
    | ToggleS3
    | ToggleS4
    | ToggleS5
    | ToggleS6
    | ToggleS7
    | ToggleS8
    | ToggleS9

type alias Model =
    { s1 : State, s2 : State, s3 : State
    , s4 : State, s5 : State, s6 : State
    , s7 : State, s8 : State, s9 : State
    , turn : Int }

initialModel : Model
initialModel =
    { s1 = Nothing, s2 = Nothing, s3 = Nothing
    , s4 = Nothing, s5 = Nothing, s6 = Nothing
    , s7 = Nothing, s8 = Nothing, s9 = Nothing
    , turn = 0}

view : Model -> Html Msg
view model =
    div []
        [
          h1 [] [ text "TicTacToe" ]
        , board model
        ]

board : Model -> Html Msg
board model =
    let
        m = model
    in
    div [ class "board" ]
        [
          cell m.s1 1
        , cell m.s2 2
        , cell m.s3 3

        , cell m.s4 4
        , cell m.s5 5
        , cell m.s6 6

        , cell m.s7 7
        , cell m.s8 8
        , cell m.s9 9
        ]


cell : State -> Int -> Html Msg
cell state sn =
    let
        stateText =
            case state of
                Nothing -> ""
                X       -> "X"
                O       -> "O"
        sig =
            case sn of
                1 -> ToggleS1
                2 -> ToggleS2
                3 -> ToggleS3
                4 -> ToggleS4
                5 -> ToggleS5
                6 -> ToggleS6
                7 -> ToggleS7
                8 -> ToggleS8
                9 -> ToggleS9
                _ -> ToggleS1 -- this will never reach

    in
    div [ class "cell", onClick sig ]
        [
          text stateText
        ]

update : Msg -> Model -> Model
update msg model =
    let
        t = if modBy 2 model.turn == 0 then X else O
    in
    case msg of
        ToggleS1 ->
            if checksOccupied model 1 then
                { model | s1 = t } |> nextTurn
            else model
        ToggleS2 ->
            if checksOccupied model 2 then
                { model | s2 = t } |> nextTurn
            else model
        ToggleS3 ->
            if checksOccupied model 3 then
                { model | s3 = t } |> nextTurn
            else model
        ToggleS4 ->
            if checksOccupied model 4 then
                { model | s4 = t } |> nextTurn
            else model
        ToggleS5 ->
            if checksOccupied model 5 then
                { model | s5 = t } |> nextTurn
            else model
        ToggleS6 ->
            if checksOccupied model 6 then
                { model | s6 = t } |> nextTurn
            else model
        ToggleS7 ->
            if checksOccupied model 7 then
                { model | s7 = t } |> nextTurn
            else model
        ToggleS8 ->
            if checksOccupied model 8 then
                { model | s8 = t } |> nextTurn
            else model
        ToggleS9 ->
            if checksOccupied model 9 then
                { model | s9 = t } |> nextTurn
            else model



checksOccupied : Model -> Int -> Bool
checksOccupied model n =
    case n of
        1 -> model.s1 == Nothing
        2 -> model.s2 == Nothing
        3 -> model.s3 == Nothing
        4 -> model.s4 == Nothing
        5 -> model.s5 == Nothing
        6 -> model.s6 == Nothing
        7 -> model.s7 == Nothing
        8 -> model.s8 == Nothing
        9 -> model.s9 == Nothing
        _ -> False -- never reaches

nextTurn : Model -> Model
nextTurn model =
    { model | turn = model.turn + 1}
