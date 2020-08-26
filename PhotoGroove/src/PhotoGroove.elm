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
          --, { url = "2.jpeg" }
          -- , { url = "2.jpeg" }
          -- , { url = "2.jpeg" }
          -- , { url = "2.jpeg" }
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


    -- case size of
    --     Small -> "small"
    --     Medium -> "medium"


-- We wrap because??
viewThumbnail2 : String -> ThumbnailSize -> Photo -> Element Msg
viewThumbnail2 selectedUrl chosenSize thumb =
    --Element.el [ Border.width <| (if selectedUrl == thumb.url then 6 else 1)
    Element.el [ Border.width <| (if selectedUrl == thumb.url then 26 else 1)
               , Border.color <| (if selectedUrl == thumb.url then blue else white)
               , Background.color (rgb 255 1 0)  --? this is not seen just put here for debugging
               ] 
               ( image [ onClick  (ClickedPhoto thumb.url)
                       , width (px (sizeToInt chosenSize))  --you are here, just got this to work, next fix spacing of thumbnails
                       ]
                     { src = urlPrefix ++ thumb.url
                     , description = ""
                     }
               )

viewThumbnail3 : String -> ThumbnailSize -> Photo -> Element Msg
viewThumbnail3 selectedUrl chosenSize thumb =
    Element.el [ padding 26 ] (
                   Element.el [ Border.width <| (if selectedUrl == thumb.url then 26 else 1)
                              , Border.color <| (if selectedUrl == thumb.url then blue else white)
                              , Background.color (rgb 255 1 0)  --? this is not seen just put here for debugging
                              ] 
                       ( image [ onClick  (ClickedPhoto thumb.url)
                               , width (px (sizeToInt chosenSize))  --you are here, just got this to work, next fix spacing of thumbnails
                               ]
                             { src = urlPrefix ++ thumb.url
                             , description = ""
                             }
                       )
                  )

        
                     
viewThumbnail : String -> ThumbnailSize -> Photo -> Element Msg
viewThumbnail selectedUrl chosenSize thumb =
    image [ Border.width <| (if selectedUrl == thumb.url then 6 else 1)
          , Border.color <| (if selectedUrl == thumb.url then blue else white)
          --, onClick  { description = "ClickedPhoto", data = thumb.url }
          , onClick  (ClickedPhoto thumb.url)
          --, width (fill |> maximum 100) --you are here, make this size chane when hit radio button
          --, width (fill |> maximum (sizeToInt chosenSize))
          , width (px (sizeToInt chosenSize))
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
        --column [ spacing 15, centerX, width (fill |> maximum 970) ]
        column [ spacing 15, centerX, width (px 960), height fill ]
        --column [ spacing 15, centerX ]
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
            --, row [ spacing 120 ] [ Element.wrappedRow [ spacingXY 10 14, width (fill |> maximum 440), alignTop ]  -- the 440 causes thumbnail to be smaller -- this is the original line
            --, row [ spacing 120 ] [ Element.wrappedRow [ spacingXY 10 14, width fill, alignTop ]
            --, row [ spacing 120 ] [ Element.wrappedRow [ spacingXY 10 14, alignTop ]
            --, row [ spacing 120 ] [ Element.wrappedRow [ spacingXY 10 14, width (fill |> maximum 700 |> minimum 400), alignTop ]
            --, row [ spacing 120
            , row [ spacing 12
                  , Background.color (rgb 0 1 0.5)  --? put here for debugging
                  , width fill
                  , height fill  --? no effect
                  ]
                --[ Element.wrappedRow [height (px 800), width (px 400)]
                --[ Element.wrappedRow [width fill, height fill]
                --[ Element.wrappedRow [alignTop, spacingXY 5 14, width fill, height fill]  --  why does row not align to top
                --[ Element.wrappedRow [alignTop, spacingXY 5 14, width fill, Border.width 1]  -- why does row not align to top, see next line
                [ Element.wrappedRow [alignTop, spacingXY 5 14, width (px 440), Border.width 1]  -- why does row not align to top, see next line
                      (List.map (viewThumbnail3 model.selectedUrl model.chosenSize)  model.photos)  -- why does relative sizeof wrapped row, and big image change even though both have 'width fill'
                      --probably because the whole thing scales, try setting fixed size above
                --[ Element.wrappedRow [width fill]
                --      [t, t, t, t, t, t, t, t, t, t]
                , image [ spacingXY 10 14
                        , alignTop
                        , alignRight
                        --?, Border.color white
                        , Border.color (rgb 1 1 0)
                        , Border.width 1
                        , width fill
                        , height fill
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


--next: things move around when changing the size of thumbnails
--      fix that
-- -thumbnails move when they are clicked
--  add second wrapper as is done here?

