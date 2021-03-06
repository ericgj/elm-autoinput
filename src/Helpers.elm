module Helpers exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Html exposing (Attribute, Html)
import Html.Attributes exposing (..)


type alias HtmlAttributeDetails =
    { attributes : List (Attribute Never)
    , style : List ( String, String )
    }


type alias HtmlDetails =
    { attributes : List (Attribute Never)
    , style : List ( String, String )
    , children : List (Html Never)
    }


nullAttribute : Attribute msg
nullAttribute =
    property "" JE.null


customDecoder : (a -> Result String b) -> JD.Decoder a -> JD.Decoder b
customDecoder f d =
    let
        resultDecoder x =
            case x of
                Ok a ->
                    JD.succeed a

                Err e ->
                    JD.fail e
    in
        JD.map f d |> JD.andThen resultDecoder


mapNeverToMsg : msg -> Attribute Never -> Attribute msg
mapNeverToMsg msg attr =
    Html.Attributes.map (\_ -> msg) attr
