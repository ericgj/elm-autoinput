module Demo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Trees exposing (ID, Tree, trees)
import Autoinput


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    Autoinput.Model ID


init : Model
init =
    Autoinput.preselect 5


type Msg
    = UpdateAutoInput (Autoinput.Msg ID)


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateAutoInput submsg ->
            Autoinput.update config trees submsg model


view : Model -> Html Msg
view model =
    div []
        [ label [] [ text "Select the tree species" ]
        , Autoinput.view config trees model |> Html.map UpdateAutoInput
        ]



-- CONSTANTS


config : Autoinput.Config Tree
config =
    Autoinput.config
        { howMany = 10
        , search = searchConfig
        , toString = (\item -> item.commonName ++ " (" ++ item.scientificName ++ ")")
        , menuId = "tree-menu"
        , menuItemStyle =
            (\selected ->
                if selected then
                    [ ( "background-color", "yellow" ) ]
                else
                    []
            )
        }


searchConfig : String -> Tree -> Bool
searchConfig q tree =
    tree.commonName
        |> String.toLower
        |> String.contains (String.toLower q)
