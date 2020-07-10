module PhotoGroove exposing (main)

-- import Html exposing (div, h1, img, text)
-- import Html.Attributes exposing (..)

-- view model =
--     div [ class "content" ]
--         [ h1 [] [ text "Photo Groove" ]
--         , div [ id "thumbnails" ]
--             [ img [ src "http://elm-in-action.com/1.jpeg" ] []
--             , img [ src "http://elm-in-action.com/2.jpeg" ] []
--             , img [ src "http://elm-in-action.com/3.jpeg" ] []
--             ]
--         ]

-- main =
--     view "no model yet"
        
import Browser
import Element exposing (..)

init = 5

       
update msg model =
    5


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
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
