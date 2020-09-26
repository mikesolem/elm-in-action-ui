module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy exposing(lazy)
import Element.Region as Region
import PhotoFolders as Folders
import PhotoGallery as Gallery
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Model =
    { page : Page, key : Nav.Key }


type Page
    = Gallery Gallery.Model
    | Folders Folders.Model
    | NotFound
        

type Route
    = Gallery
    | Folders
    | SelectedPhoto String

      
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url, key = key }, Cmd.none )


urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound
        

parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]

        
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
                underline = if isActive { link = targetPage, page = page } then [Font.underline] else []
            in
                link underline { url = url, label = text caption }
                
    in
        row [ Region.navigation, spacing 40 ]
            [ logo
            , links
            ]


isActive : { link : Page, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Gallery, Gallery ) ->
            True

        ( Gallery, _ ) ->
            False

        ( Folders, Folders ) ->
            True

        ( Folders, SelectedPhoto _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False

        ( NotFound, _ ) ->
            False


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
                
            
viewFooter : Element msg
viewFooter =
    el [ Region.footer
       , Font.color <| lightgray
       , paddingXY 20 50
       ]
        (text "One is never alone with a rubber duck. -Douglas Adams")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )
                
                        
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
        


      
        
