module Color exposing
    ( black
    , lightBlue
    , lightGray
    , white
    )

import Element


lightBlue : Element.Color
lightBlue =
    Element.rgb255 123 164 206


lightGray : Element.Color
lightGray =
    Element.rgb255 211 211 211


white : Element.Color
white =
    Element.rgb255 255 255 255


black : Element.Color
black =
    Element.rgb255 0 0 0
