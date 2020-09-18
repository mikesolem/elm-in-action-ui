module PhotoFolders exposing(main)

import Browser
import Element.Border as Border
import Element.Input as Input
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing(onClick)
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type Folder =
    Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        , expanded : Bool
        }

type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root = Folder { name = "Loading...", photoUrls = [], subfolders = [], expanded = True }
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
        , root =
            Folder
            { name = "Photos"
            , photoUrls = []
            , subfolders =
                  [ Folder
                        { name = "2016"
                        , photoUrls = [ "trevi", "coli" ]
                        , subfolders =
                              [ Folder { name = "outdoors", photoUrls = [], subfolders = [], expanded = True }
                              , Folder
                                    { name = "indoors"
                                    , photoUrls = [ "fresco" ]
                                    , subfolders = []
                                    , expanded = True
                                    }
                              ]
                        , expanded = True
                        }
                  , Folder
                        { name = "2017"
                        , photoUrls = []
                        , subfolders =
                              [ Folder
                                    { name = "outdoors"
                                    , photoUrls = []
                                    , subfolders = []
                                    , expanded = True
                                    }
                              , Folder
                                    { name = "indoors"
                                    , photoUrls = []
                                    , subfolders = []
                                    , expanded = True
                                    }
                              ]
                        , expanded = True

                        }
                  ]
            , expanded = True
                }
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
      

white =
    rgb255 0xff 0xff 0xff

gray =
    rgb255 44 44 44

blue =
    rgb255 0x60 0xb5 0xcc

           
h1 theText =
    el [ Font.size 32
       , Font.family [ Font.typeface "Verdana" ]
       , Font.color <| blue
       , Font.semiBold
       ] (text theText)


viewFolder : Folder -> Element Msg
viewFolder (Folder folder) =
    let
        subfolders =
            List.map viewFolder folder.subfolders
    in
        column [ paddingXY 20 5 ] [ Input.button [ Background.color <| rgb255 84 84 84
                                                 , Font.color <| white
                                                 , padding 7
                                                 , Font.size 18
                                                 ]
                                        { onPress = Nothing
                                        , label = text folder.name
                                        }
                  , column [] subfolders
                  ]
        

view : Model -> Html Msg
view model =
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
        Element.layout [ Background.color <| gray
                       , paddingXY 10 60
                       ] ( row [ width (px 960) ] [ column [ alignTop, width (px 360)  ]
                                                        [ h1 "Folders"
                                                        , viewFolder model.root
                                                        ]
                                  , Element.el [width (px 360)] selectedPhoto
                                  ]
                         )
            
            
    
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
    column [ spacing 2 ]
        [ Element.el [ Font.size 24
                     , Font.color <| white
                     , Font.family [ Font.typeface "Verdana" ]
                     , Font.semiBold
                     ] (text photo.title)
        , Element.el[ height (px 22)] Element.none
        , image [ Border.width 1
                , Border.color <| white
                ]
              ({ src = urlPrefix ++ "photos/" ++ photo.url ++ "/full", description = "" } )
        , Element.el [ centerX
                     , Font.color <| white
                     , Font.size 16
                     , padding 7
                     ]
              (text (String.fromInt photo.size ++ "KB"))
        , Element.el[ height (px 26)] Element.none
        , Element.el [ Font.color <| blue
                     , Font.extraBold
                     ] (text "Related")
        , Element.el[ height (px 26)] Element.none
        , row [ spacing 10 ] (List.map viewRelatedPhoto photo.relatedUrls)
        ]
    

viewRelatedPhoto : String -> Element Msg
viewRelatedPhoto url =
    Element.el [ onClick (ClickedPhoto url)
               ]
        ( image [Border.width 1
                , Border.color <| white
                ]
              ({ src = urlPrefix ++ "photos/" ++ url ++ "/thumb", description = "" } ))

           
urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

    


    
