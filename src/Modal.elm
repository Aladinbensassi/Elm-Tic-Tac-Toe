module Modal exposing (modal)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Stylesheet exposing (..)


modal : Int -> msg -> msg -> msg -> Html msg
modal boardSize modalToggle decCell addCell =
    div [ css [ modalStyle ] ]
        [ div [ css [ titleStyle ] ] [ text "Let's Play Tic Tac Toe!" ]
        , div [ onClick modalToggle, css [ playButtonStyle ] ]
            []
        , div [ css [ paragraphStyle ] ] [ text ("Board Size " ++ String.fromInt boardSize ++ "*" ++ String.fromInt boardSize) ]
        , div []
            [ button [ onClick decCell, css [ smallButtonStyle ] ] [ text "-" ]
            , button [ onClick addCell, css [ smallButtonStyle ] ] [ text "+" ]
            ]
        ]
