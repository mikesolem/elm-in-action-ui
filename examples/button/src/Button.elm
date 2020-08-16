module Button exposing (main)

import Element exposing (Element, rgb255, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

main = Element.layout []
       <| Input.button
           [ Font.color <| rgb255 44 44 44
           , Border.rounded 5
           , Element.focused
                 [ Font.color <| rgb255 44 88 44
                 --, Border.rounded 2  -- This does not compile
                 ]
           ]
           { onPress = Just "hello"
           , label = text "Hello"
           }

           
       
