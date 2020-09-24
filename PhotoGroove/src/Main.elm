module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy exposing(lazy)
import Element.Region as Region
import Url exposing (Url)


type alias Model =
    { page : Page }


type Page
    = Gallery
    | Folders
    | NotFound
        

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case url.path of
        "/gallery" ->
            ( { page = Gallery }, Cmd.none )

        "/" ->
            ( { page = Folders }, Cmd.none )

        _ ->
            ( { page = NotFound }, Cmd.none )

                    
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
                          [ lazy viewHeader model.page
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
    Browser.application
        { init = init
        , onUrlRequest = \_ -> Debug.todo "handle URL request"
        , onUrlChange = \_ -> Debug.todo "handle URL change"
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
        


      
        
