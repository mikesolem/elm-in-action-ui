module Image exposing (main)

import Element
import Html.Attributes

main = Element.layout [] ( Element.image
                               [ Element.htmlAttribute <| Html.Attributes.title "a title"
                               ]
                               { src = "http://elm-in-action.com/1.jpeg"
                               , description = "hello" }
                         )
