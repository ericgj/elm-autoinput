module Helpers exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Html exposing (Attribute)
import Html.Attributes exposing (..)


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

