module PhotoFolders exposing (Model, Msg, init, update, view)

-- timeout 1h elm-live src/PhotoFolders.elm -- --output=app.js

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


init : Maybe String -> ( Model, Cmd Msg )
init selectedFilename =
    ( { initialModel | selectedPhotoUrl = selectedFilename }
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\photos root ->
             { photos = photos, root = root, selectedPhotoUrl = Nothing }
        )
        modelPhotosDecoder
        folderDecoder
        

type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)
    | ClickedFolder FolderPath
      

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )
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


viewFolder : FolderPath -> Folder -> Element Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Element Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        arrow =
            if folder.expanded then
                "\u{25B8}" -- This is an arrow (triangle) pointing to the right
            else
                "\u{25BE}" -- This is an arrow (triangle) pointing down
                
        folderLabel = Input.button [ Background.color <| rgb255 84 84 84
                                   , Font.color <| white
                                   , padding 7
                                   , Font.size 18
                                   ]
                      { onPress = Just (ClickedFolder path)
                      , label = text (arrow ++ folder.name)
                      }
                      
    in
        if folder.expanded then
            let
                contents = List.append
                           (List.indexedMap viewSubfolder folder.subfolders)
                           (List.map viewPhoto folder.photoUrls)
            in
                column [ paddingXY 20 5 ] [ folderLabel, column [] contents]
        else
            column [ paddingXY 20 5 ] [ folderLabel ]
                            
                            
appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End
        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)

            

view : Model -> Element Msg
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
        Element.el [ Background.color <| gray
                       , paddingXY 10 60
                       ] ( row [ width (px 960) ] [ column [ alignTop, width (px 360)  ]
                                                        [ h1 "Folders"
                                                        , viewFolder End model.root
                                                        ]
                                  , Element.el [width (px 360)] selectedPhoto
                                  ]
                         )
            
            
type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


viewPhoto : String -> Element Msg
viewPhoto url =
    Input.button [ Font.color <| white
                 , padding 7
                 , Font.size 18
                 , paddingXY 20 5
                 ]
        { onPress = Just (ClickedPhoto url)
        , label = text url
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


type FolderPath
    = End
    | Subfolder Int FolderPath
        
    
toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder
                    else
                        currentSubfolder
            in
                Folder { folder | subfolders = subfolders }
                    
    
type alias JsonPhoto =
    { title : String
    , size : Int
    , relatedUrls : List String
    }


jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decode.succeed JsonPhoto
        |> required "title" string
        |> required "size" int
        |> required "related_photos" (list string)


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url = url
      , size = json.size
      , title = json.title
      , relatedUrls = json.relatedUrls
      }
    )


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs pairs =
    pairs
        |> List.map finishPhoto
        |> Dict.fromList
           
    
photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decode.keyValuePairs jsonPhotoDecoder
        |> Decode.map fromPairs


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" string
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list folderDecoder))


folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name = name
        , expanded = True
        , subfolders = subfolders
        , photoUrls = Dict.keys photos
        }
        
           
modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decode.succeed modelPhotosFromJson
        |> required "photos" photosDecoder 
        |> required "subfolders" (Decode.lazy (\_ -> list modelPhotosDecoder)) 


modelPhotosFromJson : Dict String Photo -> List (Dict String Photo) -> Dict String Photo
modelPhotosFromJson folderPhotos subfolderPhotos =
    List.foldl Dict.union folderPhotos subfolderPhotos
        

-- Dummy to make it compile        
main =
  Html.text "Hello!"           
