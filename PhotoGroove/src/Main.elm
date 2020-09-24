module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region

type alias Model =
    { page : Page }


type Page
    = Gallery
    | Folders
    | NotFound
        

init = \_ -> ( { page = Gallery }, Cmd.none )

       
gray =
    rgb255 44 44 44


lightgray =
    rgb255 187 187 187

        
        
white =
    rgb255 0xff 0xff 0xff


blue =
    rgb255 0x60 0xb5 0xcc

        
h1 theText =
    el [ Font.size 26
       , Font.family [ Font.typeface "Verdana" ]
       , Font.color <| blue
       , Font.semiBold
       ] (text theText)

        
view : Model -> Document Msg
view model =
    let
        content =  text "This isn't even my final form!"
    in
        { title = "Photo Groove, SPA Style"
        , body =
              [ layout [ Background.color <| gray
                               , paddingXY 10 30
                               , Font.color <| white
                               , Font.size 16
                               , Font.family [ Font.typeface "Verdana" ]
                               ]
                    ( column [ spacing 15 ]
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
            row [ spacing 30 ]
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink targetPage { url, caption } =
            let
                underline = if page == targetPage then [Font.underline] else []
            in
                link underline { url = url, label = text caption }
                
    in
        row [ Region.navigation, spacing 40 ]
            [ logo
            , links
            ]

            
viewFooter : Element msg
viewFooter =
    el [ Region.footer
       , Font.color <| lightgray
       , paddingXY 20 50
       ]
        (text "One is never alone with a rubber duck. -Douglas Adams")


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
        { init = \_ -> ( { page = Folders }, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
        


      
        
