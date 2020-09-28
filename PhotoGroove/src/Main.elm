module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy exposing(lazy)
import Element.Region as Region
import Html
import PhotoFolders as Folders
import PhotoGallery as Gallery
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Model =
    { page : Page
    , key : Nav.Key
    , version : Float }


type Page
    = GalleryPage Gallery.Model
    | FoldersPage Folders.Model
    | NotFound
        

type Route
    = Gallery
    | Folders
    | SelectedPhoto String

      
init : Float -> Url -> Nav.Key -> ( Model, Cmd Msg )
init version url key =
    updateUrl url { page = NotFound, key = key, version = version }


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Gallery ->
            Gallery.init model.version |> toGallery model

        Just Folders ->
            Folders.init Nothing |> toFolders model

        Just (SelectedPhoto filename) ->
            Folders.init (Just filename) |> toFolders model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


urlToPage : Float -> Url -> Page
urlToPage version url =
    case Parser.parse parser url of
        Just Gallery ->
            GalleryPage (Tuple.first (Gallery.init version))

        Just Folders ->
            FoldersPage (Tuple.first (Folders.init Nothing))

        Just (SelectedPhoto filename) ->
            FoldersPage (Tuple.first (Folders.init (Just filename)))

        Nothing ->
            NotFound
                

parser : Parser (Route -> a) a
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
        content =
            case model.page of
                FoldersPage folders ->
                    Folders.view folders
                        |> Element.map GotFoldersMsg

                GalleryPage gallery ->
                    Gallery.view gallery
                        |> Element.map GotGalleryMsg

                NotFound ->
                    text "Not Found"
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

        navLink : Route -> { url : String, caption : String } -> Element msg
        navLink route { url, caption } =
            let
                underline = if isActive { link = route, page = page } then [Font.underline] else []
            in
                link underline { url = url, label = text caption }
                
    in
        row [ Region.navigation, spacing 40 ]
            [ logo
            , links
            ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Gallery, GalleryPage _ ) ->
            True

        ( Gallery, _ ) ->
            False

        ( Folders, FoldersPage _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotFoldersMsg Folders.Msg
    | GotGalleryMsg Gallery.Msg
                
            
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
            updateUrl url model

        GotFoldersMsg foldersMsg ->
            case model.page of
                FoldersPage folders ->
                    toFolders model (Folders.update foldersMsg folders)

                _ ->
                    ( model, Cmd.none )

        GotGalleryMsg galleryMsg ->
            case model.page of
                GalleryPage gallery ->
                    toGallery model (Gallery.update galleryMsg gallery)

                _ ->
                    ( model, Cmd.none )

                        

toFolders : Model -> ( Folders.Model, Cmd Folders.Msg ) -> ( Model, Cmd Msg )
toFolders model ( folders, cmd ) =
    ( { model | page = FoldersPage folders }
    , Cmd.map GotFoldersMsg cmd
    )
                        

toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg )
toGallery model ( folders, cmd ) =
    ( { model | page = GalleryPage folders }
    , Cmd.map GotGalleryMsg cmd
    )

    
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        GalleryPage gallery ->
            Gallery.subscriptions gallery
                |> Sub.map GotGalleryMsg

        _ ->
            Sub.none


main : Program Float Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
        

-- elm make src/*.elm --output app.js
-- mike@NY80LT23PB7H2:/mnt/c/projects/git_other/elm-in-action/elm-in-action-ui/PhotoGroove$ timeout 1h http-server-spa .
-- https://awesomeopensource.com/project/izdi/elm-cheat-sheet
       
      
        
