module Demo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Trees exposing (ID,Tree,trees)
import Autoinput

main : Program Never Model Msg
main = 
  beginnerProgram
    { model = init
    , view = view
    , update = update
    }


type alias Model = Autoinput.Model ID

init : Model
init = 
  Autoinput.preselect 5

type Msg = UpdateAutoInput (Autoinput.Msg ID)

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateAutoInput submsg ->
      Autoinput.update config trees submsg model

view : Model -> Html Msg
view model =
  Autoinput.view config trees model |> Html.map UpdateAutoInput


-- CONSTANTS

config : Autoinput.Config Tree
config =
  { howMany = 5 
  , search = searchConfig
  , toString = .commonName
  , input = { attributes = [], style = [] }
  , menuConfig =
    { id = "tree-menu"
    , ul = 
      { attributes = []
      , style = [("list-style-type","none")]
      , children = [] 
      }
    , li = menuItemConfig
    }
  }

searchConfig : String -> Tree -> Bool
searchConfig q tree =
  tree.commonName
    |> String.toLower
    |> String.contains (String.toLower q)

menuItemConfig selected item =
  { attributes = []
  , style = if selected then [("background-color","yellow")] else []
  , children = [ text (item.commonName ++ " (" ++ item.scientificName ++ ")") ]
  }


