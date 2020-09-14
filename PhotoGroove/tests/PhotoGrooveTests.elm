module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Html.Attributes as Attr exposing (src)
import Test.Html.Query as Query
import Test.Html.Selector exposing(text, tag, attribute, id, containing)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import Fuzz exposing (Fuzzer, int, list, string)
import PhotoGroove exposing (Model, Msg(..), Photo, Status(..), initialModel, photoFromUrl, update, urlPrefix, view)
import Test exposing (..)
import Test.Html.Event as Event




decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")

            
sliders : Test
sliders =
    describe "Slider sets the desired field in the model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]

    
testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount
    

--?86
-- removeMe =
--     test "The <ul> only has <li> children" <|
--         \() ->
--             div []
--                 [ ul [ class "items active" ]
--                       [ li [ class "item"] [ text "first item" ]
--                       , li [ class "item selected"] [ text "second item" ]
--                       , li [ class "item"] [ text "third item" ]
--                       ]
--                 ]
--                 |> Query.fromHtml
--                 |> Query.find [ class "items" ]
--                 |> Query.children [ class "selected" ]
--                 |> Query.count (Expect.equal 1)
                   
                   
noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)



thumbnailsWork : Test
thumbnailsWork =
    fuzz urlFuzzer "URLs render as thumbnails" <|
        \urls ->
            let
                    
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls

            in
                { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                    |> view
                    |> Query.fromHtml
                    |> Expect.all thumbnailChecks
                  


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")
       

-- This is the code from the book           
-- clickThumbnail : Test
-- clickThumbnail =
--     fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
--         \urlsBefore urlToSelect urlsAfter ->
--             let
--                 url =
--                     urlToSelect ++ ".jpeg"

--                 photos =
--                     (urlsBefore ++ url :: urlsAfter) |> List.map photoFromUrl

--                 srcToClick =
--                     urlPrefix ++ url
--             in
--             { initialModel | status = Loaded photos "" }
--                 |> view
--                 |> Query.fromHtml
--                 |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
--                 |> Event.simulate Event.click
--                 |> Event.expect (ClickedPhoto url)

-- clickThumbnail : Test
-- clickThumbnail =
--     fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
--         \urlsBefore urlToSelect urlsAfter ->
--             let
--                 url =
--                     urlToSelect ++ ".jpeg"

--                 photos =
--                     (urlsBefore ++ url :: urlsAfter) |> List.map photoFromUrl

--                 srcToClick =
--                     urlPrefix ++ url
--             in
--             { initialModel | status = Loaded photos "" }
--                 |> view
--                 |> Query.fromHtml
--                 --|> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
--                 --|> Query.find [ tag "div", attribute (Attr.src srcToClick) ]
--                 |> Query.findAll [ tag "div" ]
--                 --   filter so only the div 
--                 |> Event.simulate Event.click
--                 |> Event.expect (ClickedPhoto url)

               

-- elm-ui puts code for onClick into javascript, not in the HTML
-- this should find the right image
--   Query.find [ tag "img", attribute (Attr.src srcToClick) ]
-- but we need to click on the element that wraps it

                   
clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"

                photos =
                    (urlsBefore ++ url :: urlsAfter) |> List.map photoFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                --|> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Query.find [ id "clickable"
                              , containing [ tag "img", attribute (Attr.src srcToClick) ]
                              ] --? <- you are here
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto url)
