module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import Fuzz exposing (Fuzzer, int, list, string)
import PhotoGroove exposing (Model, Msg(..), Photo, initialModel, update)
import Test exposing (..)


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

            
slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            initialModel
                |> update (SlidHue amount)
                |> Tuple.first
                |> .hue
                |> Expect.equal amount
