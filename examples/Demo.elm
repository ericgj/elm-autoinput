module Demo exposing (..)

import Json.Encode
import Html exposing (..)
import Html.Attributes exposing (style)
import Trees exposing (ID, encodeID, Tree, trees)
import Autoinput exposing (Autoinput, State(..))


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init <| List.head trees
        , view = view
        , update = update
        }


type alias Model =
    Autoinput (ID, Tree)


init : Maybe (ID, Tree) -> Model
init tree =
    tree
        |> Maybe.map (Autoinput.preselect)
        |> Maybe.withDefault Autoinput.empty


type Msg
    = UpdateAutoInput Autoinput.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateAutoInput submsg ->
            Autoinput.update config trees submsg model


view : Model -> Html Msg
view model =
    div []
      [ div [ style [("display", "inline-block"), ("width","300px")] ]
        [ label [] [ text "Select the tree species" ]
        , Autoinput.view config trees model |> Html.map UpdateAutoInput
        ]
      , div [ style [("display", "inline-block"), ("width","300px")] ]
        [ pre [ style [("color", "red")] ]
          [ text <| toString <| (Autoinput.state model)
          ]
        ]
      ]



-- CONSTANTS


config : Autoinput.Config (ID, Tree)
config =
    Autoinput.config
        { howMany = 10
        , search = searchConfig
        , toString = (\(_, item) -> item.commonName ++ " (" ++ item.scientificName ++ ")")
        , toId = (\(id, _) -> toString id)
        , menuId = "tree-menu"
        , menuItemStyle =
            (\selected ->
                if selected then
                    [ ( "background-color", "yellow" ) ]
                else
                    []
            )
        }


searchConfig : String -> (ID, Tree) -> Bool
searchConfig q (id, tree) =
    tree.commonName
        |> String.toLower
        |> String.contains (String.toLower q)

