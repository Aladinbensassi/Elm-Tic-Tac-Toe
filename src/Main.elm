-- This module defines a simple implementation of a Tic Tac Toe game.
-- It provides the user with a 3x3, 4x4, 5x5 board that can be clicked to add X or O symbols.
-- It keeps track of the current player and the game state, and displays a message to the user when the game ends.


module Main exposing (main)

import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Modal exposing (modal)
import Stylesheet exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view >> toUnstyled, update = update }


type alias Model =
    { board : Board -- The current board state, mapping the positions to players (X or O).
    , currentPlayer : Player -- The current player (X or O).
    , gameState : GameState -- The current game state (Started or Finished).
    , boardSize : Int -- The size of the board (3, 4, or 5).
    , isModalShown : Bool -- Whether to show the initial play modal.
    }



-- The `Player` type defines the two possible players (X or O).


type Player
    = X
    | O



-- The `GameState` type defines the two possible game states (Started or Finished).


type GameState
    = Started
    | Finished FinishedState


type FinishedState
    = Draw
    | Win Player



-- The `Board` type is an alias for a dictionary mapping a position (x,y) to a player (X or O).


type alias Board =
    Dict ( Int, Int ) Player



-- The initial model, when the game starts.


initialModel : Model
initialModel =
    { board = Dict.empty -- An empty board.
    , currentPlayer = X -- Player X starts.
    , gameState = Started -- The game is just starting.
    , boardSize = 3 -- A 3x3 board.
    , isModalShown = True -- Show the initial play modal.
    }


type Msg
    = SquareClick ( Int, Int ) -- A square on the board was clicked.
    | AddCell -- Increase the board size by 1.
    | DecCell -- Decrease the board size by 1.
    | PlayAgain -- Start a new game.
    | ToggleModal -- Hide the modal to change the board size.



-- This function change the player after each turn


changePlayer : Player -> Player
changePlayer player =
    case player of
        X ->
            O

        O ->
            X


update : Msg -> Model -> Model
update msg model =
    case msg of
        SquareClick ( x, y ) ->
            -- If the game has ended, do nothing.
            if model.gameState /= Started then
                model

            else
                -- Insert the new move into the board.
                let
                    newBoard =
                        Dict.insert ( x, y ) model.currentPlayer model.board
                in
                -- If the move was valid, switch players and check for a winner or a draw.
                if isSquareEmpty model.board ( x, y ) then
                    { model
                        | currentPlayer = changePlayer model.currentPlayer
                        , board = newBoard
                        , gameState =
                            if Dict.size model.board < (model.boardSize * model.boardSize - 1) && checkWinner newBoard model.boardSize == Nothing then
                                Started

                            else
                                case checkWinner newBoard model.boardSize of
                                    Just X ->
                                        Finished (Win X)

                                    Just O ->
                                        Finished (Win O)

                                    _ ->
                                        Finished Draw
                    }

                else
                    -- If the move was invalid, do nothing.
                    model

        PlayAgain ->
            -- Reset the game to the initial state.
            initialModel

        AddCell ->
            -- Increase the board size by 1.
            let
                maxBoardSize : Int
                maxBoardSize =
                    5
            in
            { model
                | boardSize =
                    if model.boardSize == maxBoardSize then
                        model.boardSize

                    else
                        model.boardSize + 1
                , board = Dict.empty
                , currentPlayer = X
            }

        DecCell ->
            -- Decrease the board size by 1.
            let
                minBoardSize : Int
                minBoardSize =
                    3

                newBoardSize =
                    if model.boardSize == minBoardSize then
                        model.boardSize

                    else
                        model.boardSize - 1
            in
            { model
                | boardSize = newBoardSize
                , board = Dict.empty
                , currentPlayer = X
            }

        ToggleModal ->
            -- Hide the modal to change the board size.
            { model | isModalShown = False }



-- Name of Player


winningPlayer : FinishedState -> String
winningPlayer state =
    case state of
        Win X ->
            "Player X"

        Win O ->
            "Player O"

        _ ->
            ""



-- Showing the symbol for each player


showPlayer : Player -> String
showPlayer player =
    case player of
        X ->
            "X"

        O ->
            "O"



-- Status of the game. Who's turn is it, whether a player has won, or there was a draw.


viewStatus : GameState -> Player -> String
viewStatus gameState currentPlayer =
    case gameState of
        Finished state ->
            case state of
                Win _ ->
                    winningPlayer state ++ " Won!"

                _ ->
                    "Draw"

        Started ->
            "Now playing: " ++ showPlayer currentPlayer



-- Board visualisation, while saving the position of each player (X, Y)


viewBoard : Int -> Board -> List (Html Msg)
viewBoard boardSize board =
    List.range 0 (boardSize - 1)
        |> List.map
            (\x ->
                div [ css [ rowStyle ] ]
                    (List.range 0 (boardSize - 1)
                        |> List.map
                            (\y ->
                                div [ onClick <| SquareClick ( x, y ), css [ squareStyle ] ]
                                    [ case Dict.get ( x, y ) board of
                                        Just player ->
                                            text <| showPlayer player

                                        Nothing ->
                                            text <| ""
                                    ]
                            )
                    )
            )


view : Model -> Html Msg
view model =
    div [ css [ mainStyle ] ]
        [ if model.isModalShown then
            modal model.boardSize ToggleModal DecCell AddCell

          else
            div [] []
        , div [ css [ titleStyle ] ] [ text <| viewStatus model.gameState model.currentPlayer ]
        , div [] <| viewBoard model.boardSize model.board
        , case model.gameState of
            Started ->
                div [] []

            Finished _ ->
                button [ css [ playAgainButtonStyle ], onClick PlayAgain ] [ text "Play Again" ]
        ]



-- Reusable Function


indices : Int -> List Int
indices boardSize =
    List.range 0 (boardSize - 1)



-- Function to get a row from the board


row : Int -> Int -> List ( Int, Int )
row boardSize rowIndex =
    indices boardSize
        |> List.map (\columnIndex -> ( columnIndex, rowIndex ))



-- Function to get a column from the board


collumn : Int -> Int -> List ( Int, Int )
collumn boardSize columnIndex =
    indices boardSize
        |> List.map (\rowIndex -> ( columnIndex, rowIndex ))



-- Function to get the diagonal from the board (top left to bottom right)


diagonal1 : Int -> List ( Int, Int )
diagonal1 boardSize =
    indices boardSize
        |> List.map (\i -> ( i, i ))



-- Function to get the diagonal from the board (top right to bottom left)


diagonal2 : Int -> List ( Int, Int )
diagonal2 boardSize =
    indices boardSize
        |> List.map (\i -> ( i, boardSize - 1 - i ))



-- Function to get all the winning positions


winPositions : Int -> List (List ( Int, Int ))
winPositions boardSize =
    (indices boardSize
        |> List.map (\y -> row boardSize y)
    )
        ++ (indices boardSize
                |> List.map (\x -> collumn boardSize x)
           )
        ++ [ diagonal1 boardSize, diagonal2 boardSize ]



-- Function to check if all elements in a list are equal


allEqual : Board -> List ( Int, Int ) -> Maybe Player
allEqual board l =
    case l of
        [] ->
            Nothing

        h :: t ->
            if List.all (\x -> Dict.get h board == Dict.get x board) t then
                Dict.get h board

            else
                Nothing



-- Function to check if a square in the board is empty (i.e. contains 0)


isSquareEmpty : Board -> ( Int, Int ) -> Bool
isSquareEmpty board ( x, y ) =
    case Dict.get ( x, y ) board of
        Just _ ->
            False

        Nothing ->
            True



-- Function to check if a player has won the game


checkWinner : Board -> Int -> Maybe Player
checkWinner board boardSize =
    winPositions boardSize
        |> List.map (allEqual board)
        |> List.filter
            (\x ->
                case x of
                    Just _ ->
                        True

                    _ ->
                        False
            )
        |> List.head
        |> Maybe.withDefault Nothing
