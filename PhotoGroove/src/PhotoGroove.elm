port module PhotoGroove exposing (main)

-- elm-live src/PhotoGroove.elm -- --output=app.js

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing(onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as Attr exposing (class, max, id, title)
import Html.Events exposing (on)  --? is this doable in elm-ui?
import List
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


type ThumbnailSize
    = Small
    | Medium
    | Large


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg                  
                  

type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , activity : String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , activity = ""
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


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
    | GotActivity String
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


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
                                 , Element.htmlAttribute <| Attr.title thumb.title
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
                     viewLoaded photos selectedUrl model

                 Loading ->
                     []

                 Errored errorMessage ->
                     [Element.text ("Error: " ++ errorMessage)]
            )


viewFilter  : (Int -> Msg) -> String -> Int -> Element Msg
viewFilter toMsg name magnitude =
    row [Font.color <| white]
        [ Element.el [ width (px 80) ] (Element.text name)
        , rangeSlider [ Attr.max "11"
                      , Attr.property "val" (Encode.int magnitude)
                      , onSlide toMsg
                      ]
              []
        , Element.text (String.fromInt magnitude)
        ]


viewLoaded : List Photo -> String -> Model -> List (Element Msg)
viewLoaded photos selectedUrl model =
    [ row [width fill] [ h1 "Photo Groove"
             , el [alignRight] (Element.text model.activity)
             ]
    , row [width fill] [ viewSizeChooser model.chosenSize
                       , column [ padding 30 ] [ viewFilter SlidHue "Hue" model.hue
                                               , viewFilter SlidRipple "Ripple" model.ripple
                                               , viewFilter SlidNoise "Noise" model.noise
                                   ]
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
                (List.map (viewThumbnail selectedUrl model.chosenSize)  photos)
          , html <| Html.canvas [ id "main-canvas", class "large" ] []
          ]
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }  

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }  

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )
                  
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

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    applyFilters { model | status =
                                       case List.head photos of
                                           Just photo ->
                                               Loaded photos photo.url
                                           Nothing ->
                                               Loaded [] ""
                                       }

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url = urlPrefix ++ "large/" ++ selectedUrl
            in
                (model, setFilters { url = url, filters = filters })

        Loading ->
            ( model, Cmd.none )

        Errored errorMessage ->
            ( model, Cmd.none )
                

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
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity
        

rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Element msg
rangeSlider attributes children =
    Element.html <| Html.node "range-slider" attributes children


onSlide : (Int -> msg) -> Html.Attribute msg
onSlide toMsg =
    let
        detailUserSlidTo : Decoder Int
        detailUserSlidTo = at [ "detail", "userSlidTo" ] int

        msgDecoder : Decoder msg
        msgDecoder = Json.Decode.map toMsg detailUserSlidTo
    in
        on "slide" msgDecoder
