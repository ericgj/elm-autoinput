module Demo exposing (Model, Msg(..), config, encode, init, main, searchConfig, update, view)

import Autoinput exposing (State(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Json.Encode
import Trees exposing (ID, Tree, encodeID, trees)


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



{-

   An example of how you might use Autoinput.state : encoding autocomplete
   models for external storage.

-}


encode : Model -> Json.Encode.Value
encode model =
    Autoinput.state model
        |> (\state ->
                case state of
                    NoInput ->
                        Json.Encode.object
                            [ ( "constructor", Json.Encode.string "NoInput" ) ]

                    Entered query ->
                        Json.Encode.object
                            [ ( "constructor", Json.Encode.string "Entered" )
                            , ( "query", Json.Encode.string query )
                            ]

                    Selected id ->
                        Json.Encode.object
                            [ ( "constructor", Json.Encode.string "Selected" )
                            , ( "id", encodeID id )
                            ]
           )


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
        [ div [ style [ ( "display", "inline-block" ), ( "width", "300px" ) ] ]
            [ label [] [ text "Select the tree species" ]
            , Autoinput.view config trees model |> Html.map UpdateAutoInput
            ]
        , div [ style [ ( "display", "inline-block" ), ( "width", "300px" ) ] ]
            [ pre [ style [ ( "color", "red" ) ] ]
                [ text <| toString <| Autoinput.state model
                ]
            ]
        ]



-- CONSTANTS


config : Autoinput.Config Tree
config =
    Autoinput.config
        { howMany = 10
        , search = searchConfig
        , toString = \item -> item.commonName ++ " (" ++ item.scientificName ++ ")"
        , menuId = "tree-menu"
        , menuItemStyle =
            \selected ->
                if selected then
                    [ ( "background-color", "yellow" ) ]

                else
                    []
        }


searchConfig : String -> Tree -> Bool
searchConfig q tree =
    tree.commonName
        |> String.toLower
        |> String.contains (String.toLower q)
