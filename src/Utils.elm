module Utils exposing (..)

import Html.Attributes exposing (attribute, href, property, style)
import Json.Encode

add_class cls =
    property "className" (Json.Encode.string cls)

