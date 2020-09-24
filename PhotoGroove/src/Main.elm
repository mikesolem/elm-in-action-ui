module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
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

       
gray =
    rgb255 44 44 44

        
white =
    rgb255 0xff 0xff 0xff


blue =
    rgb255 0x60 0xb5 0xcc

        
h1 theText =
    el [ Font.size 26
       , Font.family [ Font.typeface "Verdana" ]
       , Font.color <| blue
       , Font.semiBold
       ] (Element.text theText)

        
view : Model -> Document Msg
view model =
    let
        content =  Element.text "This isn't even my final form!"
    in
        { title = "Photo Groove, SPA Style"
        , body =
              [ Element.layout [ Background.color <| gray
                               , paddingXY 10 30
                               , Font.color <| white
                               ]
                    ( Element.column [ ]
                          [ viewHeader model.page
                          , content
                          , viewFooter
                          ]
                    )
              ]
        }


viewHeader : Page -> Element msg
viewHeader page =
    let
        logo =
            h1 "Photo Groove"

        links =
            Element.row [ spacing 30 ]
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        
                
        navLink targetPage { url, caption } =
            Element.link [ Font.underline ] { url = url, label = Element.text caption }
                
    in
        Element.row [ Region.navigation, spacing 40 ]
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
        
        
      
        
