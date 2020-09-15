module PhotoFolders exposing(main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Events exposing(onClick)
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos =
            Dict.fromList
                [ ( "trevi"
                  , { title = "Trevi"
                    , relatedUrls = [ "coli", "fresco" ]
                    , size = 34
                    , url = "trevi"
                    }
                  )
                , ( "fresco"
                  , { title = "Fresco"
                    , relatedUrls = [ "trevi" ]
                    , size = 46
                    , url = "fresco"
                    }
                  )
                , ( "coli"
                  , { title = "Coliseum"
                    , relatedUrls = [ "trevi", "fresco" ]
                    , size = 36
                    , url = "coli"
                    }
                  )
                ]
        }
        

type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )
      


blue =
    rgb255 0x60 0xb5 0xcc

           
h1 theText =
    el [ Font.size 32
       , Font.family [ Font.typeface "Verdana" ]
       , Font.color <| blue
       , Font.semiBold
       ] (text theText)


view : Model -> Html Msg
view model =
    --Element.layout [] ( h1 "The Grooviest Folders the world has ever seen" )
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Element Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
        Element.layout [] (selectedPhoto)
            
    
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }            


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


viewSelectedPhoto : Photo -> Element Msg
viewSelectedPhoto photo =
    column []
        [ text photo.title
        , image [] ({ src = urlPrefix ++ "photos/" ++ photo.url ++ "/full", description = "" } )
        , text (String.fromInt photo.size ++ "KB")
        , text "Related"
        , row [] (List.map viewRelatedPhoto photo.relatedUrls)
        ]
    

viewRelatedPhoto : String -> Element Msg
viewRelatedPhoto url =
    Element.el [ onClick (ClickedPhoto url)
               ]
        ( image [] ({ src = urlPrefix ++ "photos/" ++ url ++ "/thumb", description = "" } ))

           
urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

    


    
