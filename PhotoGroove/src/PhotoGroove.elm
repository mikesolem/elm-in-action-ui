module PhotoGroove exposing (main)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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
    image [ Border.width 1
          , Border.color <| (if selectedUrl == thumb.url then blue else white)
          ]
          { src = urlPrefix ++ thumb.url
          , description = ""
          }


view model =
    layout [ Background.color <| rgb255 44 44 44
           , paddingXY 10 40
           ] <|
        column [ spacing 30 ]
            [ el [ Font.size 32
                 , Font.family [ Font.typeface "Verdana" ]
                 , Font.color <| blue
                 , Font.semiBold
                 ] (text "Photo Groove")
            , Element.wrappedRow [ spacingXY 10 14, width (fill |> maximum 440) ]
                (List.map (viewThumbnail model.selectedUrl)  model.photos)
            ]


main =
    view initialModel
