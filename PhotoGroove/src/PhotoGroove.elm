module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing(onClick)
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import List


type alias Photo =
    { url : String }
        

type alias Model =
    { photos : List Photo
    , selectedUrl : String
    }
        

initialModel : Model
initialModel =
    { photos =
          [ { url = "1.jpeg" }
          , { url = "2.jpeg" }
          , { url = "3.jpeg" }
          ]
    , selectedUrl = "1.jpeg"
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos
        
    
urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


white =
    rgb255 0xff 0xff 0xff


blue =
    rgb255 0x60 0xb5 0xcc


type alias Msg =
    { description : String, data : String }

        
viewThumbnail : String -> Photo -> Element Msg
viewThumbnail selectedUrl thumb =
    image [ Border.width <| (if selectedUrl == thumb.url then 6 else 1)
          , Border.color <| (if selectedUrl == thumb.url then blue else white)
          , onClick  { description = "ClickedPhoto", data = thumb.url }
          ]
          { src = urlPrefix ++ thumb.url
          , description = ""
          }


h1 theText =
    el [ Font.size 32
       , Font.family [ Font.typeface "Verdana" ]
       , Font.color <| blue
       , Font.semiBold
       ] (text theText)
        

view : Model -> Html Msg
view model =
    layout [ Background.color <| rgb255 44 44 44
           , paddingXY 10 40
           ] <|
        column [ spacing 30 ]
            [ h1 "Photo Groove"
            , row [] [ Element.wrappedRow [ spacingXY 10 14, width (fill |> maximum 440), alignTop ]
                           (List.map (viewThumbnail model.selectedUrl)  model.photos)
                     , image [ spacingXY 10 14
                             , Border.color white
                             , Border.width 1 ]
                           { src = urlPrefix ++ "large/" ++ model.selectedUrl
                           , description = ""
                           }
                     ]
            ]


update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }
    else
        model


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
