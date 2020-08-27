module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing(onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import List


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { photos =
          [ { url = "1.jpeg" }
          , { url = "2.jpeg" }
          , { url = "3.jpeg" }
          ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


gray =
    rgb255 44 44 44

        
white =
    rgb255 0xff 0xff 0xff


blue =
    rgb255 0x60 0xb5 0xcc


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe




viewThumbnail4 : String -> ThumbnailSize -> Photo -> Element Msg
viewThumbnail4 selectedUrl chosenSize thumb =
    let
        width_ = sizeToInt chosenSize
                 
        highlight = if selectedUrl == thumb.url then True else False
                    
        (outerBorderColor, innerBorderColor) = if highlight then
                                                   (blue, blue)
                                               else
                                                   (gray, white)
    in
        Element.el []
            ( Element.el [ Border.width 5
                         , Border.color outerBorderColor
                         ]
                  ( Element.el [ Border.width 1
                               , Border.color innerBorderColor
                               , onClick  (ClickedPhoto thumb.url)
                               ]
                        ( image  [ width (px width_)
                                 , centerX
                                 , centerY
                                 ]
                              { src = urlPrefix ++ thumb.url, description = "" }
                        )))
        
                     
viewSizeChooser  size =
    Input.radioRow
        [ Font.color <| white ]
        { label = Input.labelLeft [ Font.color <| white
                                  , Font.family [ Font.typeface "Verdana" ]
                                  , Font.semiBold
                                  ]
              (text "Thumbnail Size:    ")
        , onChange = ClickedSize 
        , selected = Just size
        , options =
            [ Input.option Small (text "small")
            , Input.option Medium (text "medium")
            , Input.option Large (text "large")
            ]
        }


--? remove if unused        
sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "medium"
        Large -> "large"

                 
sizeToInt : ThumbnailSize -> Int
sizeToInt size =
    case size of
        Small -> 50
        Medium -> 100
        Large -> 200

             
h1 theText =
    el [ Font.size 32
       , Font.family [ Font.typeface "Verdana" ]
       , Font.color <| blue
       , Font.semiBold
       ] (text theText)


t = Element.text "Hello" 

    
view : Model -> Html Msg
view model =
    layout [ Background.color <| rgb255 44 44 44
           , paddingXY 10 45
           ] <|
        column [ spacing 15, centerX, width (px 960), height fill ]
            [ h1 "Photo Groove"
            , row [width fill] [ viewSizeChooser model.chosenSize
                     , Input.button
                           [ alignRight
                           , Background.color <| blue
                           , Font.color <| rgb255 44 44 44
                           , paddingXY 30 10
                           , Font.size 22
                           , Font.family [ Font.typeface "Verdana" ]
                           , mouseOver [ Background.color <| white ]
                           , Border.width 1
                           , Element.focused [ Border.color <| white ]
                           ]
                           { onPress = Just ClickedSurpriseMe
                           , label = text "Surprise Me!"
                           }
                     ]
                
            , row [ spacing 12
                  , width fill
                  ]
                  [ Element.wrappedRow [alignTop, spacingXY 0 14, width (px 440) ]
                        (List.map (viewThumbnail4 model.selectedUrl model.chosenSize)  model.photos)
                  , image [ spacingXY 10 14
                          , alignTop
                          , alignRight
                          , Border.color white
                          , Border.width 1
                          , width fill
                          ]
                        { src = urlPrefix ++ "large/" ++ model.selectedUrl
                        , description = ""
                        }
                  ]
            ]


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""
                
            
update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto url ->
            { model | selectedUrl = url }

        ClickedSize size ->
            { model | chosenSize = size }

        ClickedSurpriseMe ->
            { model | selectedUrl = "2.jpeg" }


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


