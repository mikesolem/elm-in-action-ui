module PhotoGroove exposing (main)

import Element exposing (..)
import Element.Font as Font
import Element.Region as Region

view model =
    layout [] <|
        column [ ]
            [ el [Font.size 32] (text "Photo Groove")
            , row [ ]
                [ image [ height fill ] {src="http://elm-in-action.com/1.jpeg", description="first"}
                , image [ height fill ] {src="http://elm-in-action.com/2.jpeg", description="second"}
                , image [ height fill ] {src="http://elm-in-action.com/3.jpeg", description="third"}  ]
            ]


main =
    view "no view yet"
