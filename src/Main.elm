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
    | PlayAgain

type alias Model =
    { s1 : State, s2 : State, s3 : State
    , s4 : State, s5 : State, s6 : State
    , s7 : State, s8 : State, s9 : State
    , turn : Int
    , winner : Bool }

initialModel : Model
initialModel =
    { s1 = Nothing, s2 = Nothing, s3 = Nothing
    , s4 = Nothing, s5 = Nothing, s6 = Nothing
    , s7 = Nothing, s8 = Nothing, s9 = Nothing
    , turn = 0
    , winner = False }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [
          h1 [] [ text "TicTacToe" ]
        , board model
        , winnerBanner (getWinner model)
        , playAgain
        -- , if drawOrWin model then playAgain model else nada
        ]

board : Model -> Html Msg
board model =
    let
        m = model
    in
    div [ class "board" ]
        [
          cell "top-left"  m.s1 1
        , cell "top-mid"   m.s2 2
        , cell "top-right" m.s3 3

        , cell "mid-left"  m.s4 4
        , cell "mid-mid"   m.s5 5
        , cell "mid-right" m.s6 6

        , cell "bot-left"  m.s7 7
        , cell "bot-mid"   m.s8 8
        , cell "bot-right" m.s9 9
        ]

winnerBanner : String -> Html Msg
winnerBanner winner =
    let
        banner = if winner /= "" then text ("Congratulations " ++ winner ++ " player!") else nada
    in
        h2 [ class "banner" ] [ banner ]

getWinner : Model -> String
getWinner model =
    if model.winner == True then
        if modBy 2 model.turn == 1 then "X" else "O"
    else ""

playAgain : Html Msg
playAgain =
    div [ class "play-again", onClick PlayAgain ]
        [
          p [ onClick PlayAgain ] [ text "Play Again?" ]
        ]

nada : Html Msg
nada = div [] []

cell : String -> State -> Int -> Html Msg
cell cssClass state sn =
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
    div [ class "cell", class cssClass, onClick sig ]
        [
          text stateText
        ]

-- UPDATE

update : Msg -> Model -> Model
update msg model =
    let
        t = if modBy 2 model.turn == 0 then X else O
    in
    if model.winner then
        model
    else
    case msg of
        ToggleS1 ->
            if checksOccupied model 1 then
                { model | s1 = t } |> goNext
            else model
        ToggleS2 ->
            if checksOccupied model 2 then
                { model | s2 = t } |> goNext
            else model
        ToggleS3 ->
            if checksOccupied model 3 then
                { model | s3 = t } |> goNext
            else model
        ToggleS4 ->
            if checksOccupied model 4 then
                { model | s4 = t } |> goNext
            else model
        ToggleS5 ->
            if checksOccupied model 5 then
                { model | s5 = t } |> goNext
            else model
        ToggleS6 ->
            if checksOccupied model 6 then
                { model | s6 = t } |> goNext
            else model
        ToggleS7 ->
            if checksOccupied model 7 then
                { model | s7 = t } |> goNext
            else model
        ToggleS8 ->
            if checksOccupied model 8 then
                { model | s8 = t } |> goNext
            else model
        ToggleS9 ->
            if checksOccupied model 9 then
                { model | s9 = t } |> goNext
            else model
        PlayAgain ->
            initialModel


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

goNext : Model -> Model
goNext model =
    model |> nextTurn |> checkWinner

nextTurn : Model -> Model
nextTurn model =
    { model | turn = model.turn + 1}

checkWinner : Model -> Model
checkWinner model =
    let
        m = model
    in
    -- rows
    if List.all (\s -> s == X) [m.s1, m.s2, m.s3]
    || List.all (\s -> s == O) [m.s1, m.s2, m.s3]
    || List.all (\s -> s == X) [m.s4, m.s5, m.s6]
    || List.all (\s -> s == O) [m.s4, m.s5, m.s6]
    || List.all (\s -> s == X) [m.s7, m.s8, m.s9]
    || List.all (\s -> s == O) [m.s7, m.s8, m.s9]
    -- now columns
    || List.all (\s -> s == X) [m.s1, m.s4, m.s7]
    || List.all (\s -> s == O) [m.s1, m.s4, m.s7]
    || List.all (\s -> s == X) [m.s2, m.s5, m.s8]
    || List.all (\s -> s == O) [m.s2, m.s5, m.s8]
    || List.all (\s -> s == X) [m.s3, m.s6, m.s9]
    || List.all (\s -> s == O) [m.s3, m.s6, m.s9]
    -- diag
    || List.all (\s -> s == X) [m.s1, m.s5, m.s9]
    || List.all (\s -> s == O) [m.s1, m.s5, m.s9]
    || List.all (\s -> s == X) [m.s7, m.s5, m.s3]
    || List.all (\s -> s == O) [m.s7, m.s5, m.s3]
    then { model | winner = True } else model

drawOrWin : Model -> Bool
drawOrWin model =
    let
        m = model
    in
    if m.winner == True then True
    else
    if List.all (\c -> c /= Nothing) [m.s1, m.s2, m.s3, m.s4, m.s5, m.s6, m.s7, m.s8, m.s9]
        then True else False
