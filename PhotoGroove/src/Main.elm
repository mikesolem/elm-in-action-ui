module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Region as Region
--import Html expso
import Html exposing (Html, a, footer, h1, li, nav, text, ul)  --? remove this
import Html.Attributes exposing (classList, href)  --? remove

type alias Model =
    { page : Page }


type Page
    = Gallery
    | Folders
    | NotFound
        

init = \_ -> ( { page = Gallery }, Cmd.none )

       
-- view : Model -> Document Msg
-- view model =
--     let
--         content = Element.layout [] ( Element.text "This isn't even my final form!x" )
--     in
--         { title = "Photo Groove, SPA Style"
--         , body =
--               [ viewHeader model.page
--               , Element.layout [] ( Element.text "This isn't even my final form! one" )
--               , Element.layout [] ( Element.text "This isn't even my final form! two" )
--               , content
--               , viewFooter
--               ]
--         }
--
-- view : Model -> Document Msg
-- view model =
--     let
--         content = Element.layout [] ( Element.text "This isn't even my final form!x" )
--     in
--         { title = "Photo Groove, SPA Style"
--         , body =
--               [ Element.layout [] ( Element.text "This isn't even my final form! one" )
--               , Element.layout [] ( Element.text "This isn't even my final form! two" )
--               ]
--         }
view : Model -> Document Msg
view model =
    let
        content =  Element.text "This isn't even my final form!"
    in
        { title = "Photo Groove, SPA Style"
        , body =
              [ Element.layout []
                    ( Element.column []
                          [ viewHeader model.page
                          --, Element.text "This isn't even my final form! one" 
                          --, Element.text "This isn't even my final form! two"
                          , content
                          , viewFooter
                          ]
                    )
              ]
        }


-- viewHeader : Page -> Html Msg
-- viewHeader page =
--     let
--         logo =
--             Element.layout [] ( Element.text "Photo Groove" ) --? factor out all this Element.layout stuff

--         links =
--             ul []
--                 [ navLink Folders { url = "/", caption = "Folders" }
--                 , navLink Gallery { url = "/gallery", caption = "Gallery" }
--                 ]

--         navLink : Page -> { url : String, caption : String } -> Html msg
--         navLink targetPage { url, caption } =
--             li []
--                 [ a [ href url ] [ text caption ] ] 
                
--     in
--         nav [] [ logo, links ]
-- viewHeader : Page -> Html Msg
-- viewHeader page =
--     let
--         logo =
--             Element.text "Photo Groove"

--         links =
--             ul []
--                 [ navLink Folders { url = "/", caption = "Folders" }
--                 , navLink Gallery { url = "/gallery", caption = "Gallery" }
--                 ]

--         navLink : Page -> { url : String, caption : String } -> Html msg
--         navLink targetPage { url, caption } =
--             li []
--                 [ a [ href url ] [ text caption ] ] 
                
--     in
--         nav [] [ logo, links ]
viewHeader : Page -> Element msg
viewHeader page =
    let
        logo =
            Element.text "Photo Groove"

        links =
            Element.row []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink targetPage { url, caption } =
            Element.link [] { url = url, label = Element.text caption }
                
    in
        Element.row [ Region.navigation ]
            [ logo
            , links
            --, Element.link [] { url = "http://bla.com", label = Element.text "not a link" }
            ]

-- viewFooter : Html msg
-- viewFooter =
--     Element.layout [] ( Element.text "One is never alone with a rubber duc. -Douglas Adams" )  --? use footer
viewFooter : Element msg
viewFooter =
    Element.text "One is never alone with a rubber duck. -Douglas Adams"  --? use footer


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        --?{ init = \_ -> ( { page = Folders }, Cmd.none )
        { init = \_ -> ( { page = Gallery }, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
        
        
      
        
