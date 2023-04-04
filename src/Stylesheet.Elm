module Stylesheet exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


mainStyle : Style
mainStyle =
    batch
        [ fontWeight normal
        , color (rgb 55 55 55)
        , displayFlex
        , flexDirection Css.column
        , justifyContent center
        , alignItems center
        , Css.height (vh 100)
        , Css.property "gap" "40px"
        ]


rowStyle : Style
rowStyle =
    batch
        [ displayFlex
        , flexDirection Css.row
        , alignItems center
        ]


squareStyle : Style
squareStyle =
    batch
        [ border3 (px 2) solid (hex "0C0C0C")
        , Css.width (px 120)
        , Css.height (px 120)
        , fontSize (px 80)
        , fontWeight bolder
        , color (hex "F2F2F2")
        , textAlign center
        , verticalAlign middle
        , backgroundColor (hex "222222")
        , cursor pointer
        , lineHeight (px 120)
        , alignItems center
        , hover [ backgroundColor (hex "383838") ]
        ]


titleStyle : Style
titleStyle =
    batch
        [ fontSize (px 28)
        , color (hex "F2F2F2")
        ]


paragraphStyle : Style
paragraphStyle =
    batch
        [ fontSize (px 18)
        , color (hex "F2F2F2")
        ]


smallButtonStyle : Style
smallButtonStyle =
    batch
        [ Css.width (px 40)
        , Css.height (px 40)
        , backgroundColor (hex "E75B4F")
        , textAlign center
        , color (hex "F2F2F2")
        , border (px 0)
        , cursor pointer
        , fontSize (px 28)
        , hover [ backgroundColor (hex "fc6a5f") ]
        ]


playAgainButtonStyle : Style
playAgainButtonStyle =
    batch
        [ Css.width (px 140)
        , Css.height (px 40)
        , backgroundColor (hex "222")
        , textAlign center
        , color (hex "F2F2F2")
        , border (px 0)
        , cursor pointer
        , fontSize (px 18)
        , boxShadow5 (px 0) (px 4) (px 9) (px 4) (rgba 0 0 0 0.75)
        , position relative
        , hover [ backgroundColor (hex "383838") ]
        ]


modalStyle : Style
modalStyle =
    batch
        [ Css.width (vw 100)
        , Css.height (vh 100)
        , backgroundColor (hex "111")
        , opacity (num 0.9)
        , displayFlex
        , flexDirection Css.column
        , justifyContent center
        , position absolute
        , zIndex (int 99)
        , alignItems center
        , top (px 0)
        , Css.property "gap" "30px"
        ]


playButtonStyle : Style
playButtonStyle =
    batch
        [ Css.width (px 60)
        , Css.height (px 60)
        , borderRadius (px 60)
        , displayFlex
        , justifyContent center
        , backgroundColor (hex "E75B4F")
        , alignItems center
        , cursor pointer
        , hover [ backgroundColor (hex "fc6a5f") ]
        , before
            [ position absolute
            , borderLeft3 (px 30) solid (hex "F2F2F2")
            , borderTop3 (px 15) solid transparent
            , borderBottom3 (px 15) solid transparent
            , Css.property "content" "''"
            ]
        ]
