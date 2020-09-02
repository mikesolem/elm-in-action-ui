module PhotoGroove exposing (main)

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
import Http
import Random


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


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
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)



viewThumbnail : String -> ThumbnailSize -> Photo -> Element Msg
viewThumbnail selectedUrl chosenSize thumb =
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


view : Model -> Html Msg
view model =
    layout [ Background.color <| rgb255 44 44 44
           , paddingXY 10 45
           ] <|
        column [ spacing 15, centerX, width (px 960), height fill ]
            (case model.status of
                 Loaded photos selectedUrl ->
                     viewLoaded photos selectedUrl model.chosenSize

                 Loading ->
                     [Element.text "Hello"]

                 Errored errorMessage ->
                     [Element.text ("Error: " ++ errorMessage)]
            )


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Element Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 "Photo Groove"
    , row [width fill] [ viewSizeChooser chosenSize
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
          [ Element.wrappedRow [alignTop, spacingXY 0 3, width (px 440) ]
                (List.map (viewThumbnail selectedUrl chosenSize)  photos)
          , image [ spacingXY 10 14
                  , alignTop
                  , alignRight
                  , Border.color white
                  , Border.width 1
                  , width fill
                  ]
                { src = urlPrefix ++ "large/" ++ selectedUrl
                , description = ""
                }
          ]
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                          |> Random.generate GotRandomPhoto
                          |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

        GotPhotos (Ok responseStr) ->
            case String.split "," responseStr of
                (firstUrl :: _) as urls ->
                    let
                        photos = List.map Photo urls
                    in
                        ( { model | status = Loaded photos firstUrl }, Cmd.none )
                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )



selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored errorMessage ->
            status


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString GotPhotos
        }

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
