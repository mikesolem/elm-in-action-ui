module PhotoGroove exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing(onClick)
import Element.Font as Font
import Element.Region as Region

import List


initialModel =
    { photos =
          [ { url = "1.jpeg" }
          , { url = "2.jpeg" }
          , { url = "3.jpeg" }
          ]
    , selectedUrl = "1.jpeg"
    }


urlPrefix =
    "http://elm-in-action.com/"


white =
    rgb255 0xff 0xff 0xff


blue =
    rgb255 0x60 0xb5 0xcc


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
