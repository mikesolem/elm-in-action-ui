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


white =
    rgb255 0xff 0xff 0xff


blue =
    rgb255 0x60 0xb5 0xcc


-- type alias Msg =
--     { description : String, data : String }
type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe


viewThumbnail : String -> Photo -> Element Msg
viewThumbnail selectedUrl thumb =
    image [ Border.width <| (if selectedUrl == thumb.url then 6 else 1)
          , Border.color <| (if selectedUrl == thumb.url then blue else white)
          --, onClick  { description = "ClickedPhoto", data = thumb.url }
          , onClick  (ClickedPhoto thumb.url)
          , width (fill |> maximum 100)
          ]
          { src = urlPrefix ++ thumb.url
          , description = ""
          }


dummyOnChange x =
    --{ description = "dummy", data = "none" }
    --ClickedSize Medium
    ClickedSize x

--onChangeB x =


viewSizeChooser  size =
    Input.radioRow
        [ Font.color <| white ]
        { label = Input.labelLeft [ Font.color <| white
                                  , Font.family [ Font.typeface "Verdana" ]
                                  , Font.semiBold
                                  ]
              (text "Thumbnail Size:    ")
        , onChange = dummyOnChange
        --, onChange = ClickedSize size
        --, selected = Just Medium
        , selected = Just size
        , options =
            [ Input.option Small (text "small")
            , Input.option Medium (text "medium")
            , Input.option Large (text "large")
            ]
        }


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "medium"
        Large -> "large"


h1 theText =
    el [ Font.size 32
       , Font.family [ Font.typeface "Verdana" ]
       , Font.color <| blue
       , Font.semiBold
       ] (text theText)


view : Model -> Html Msg
view model =
    layout [ Background.color <| rgb255 44 44 44
           , paddingXY 10 45
           ] <|
        column [ spacing 15, centerX ]
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
                           --{ onPress = Just { description = "ClickedSurpriseMe", data = "" }
                           { onPress = Just ClickedSurpriseMe
                           , label = text "Surprise Me!"
                           }
                     ]
            , row [ spacing 120 ] [ Element.wrappedRow [ spacingXY 10 14, width (fill |> maximum 440), alignTop ]
                           (List.map (viewThumbnail model.selectedUrl)  model.photos)
                     , image [ spacingXY 10 14
                             , Border.color white
                             , Border.width 1
                             ]
                           { src = urlPrefix ++ "large/" ++ model.selectedUrl
                           , description = ""
                           }
                     ]
            ]


-- update : Msg -> Model -> Model
-- update msg model =
--     case msg.description of
--         "ClickedPhoto" ->
--             { model | selectedUrl = msg.data }
--         "ClickedSurpriseMe" ->
--             { model | selectedUrl = "2.jpeg" }
--         _ ->
--             model
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
