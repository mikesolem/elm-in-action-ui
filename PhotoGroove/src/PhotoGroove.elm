module PhotoGroove exposing (main)
        
import Element exposing (..)


view model =
    layout [] <|
        column [ height fill, width fill ]
            [ text "Photo Groove"
            , row [ height fill, width fill ]
                [ image [ height fill ] {src="http://elm-in-action.com/1.jpeg", description="first"}
                , image [ height fill ] {src="http://elm-in-action.com/2.jpeg", description="second"}
                , image [ height fill ] {src="http://elm-in-action.com/3.jpeg", description="third"}  ]
            ]
    

main =
    view "no view yet"
        
