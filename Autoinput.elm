module UI.Autoinput 
  exposing 
    ( Model
    , Config
    , init
    , empty
    , Msg
    , findById
    , update
    , view
    , defaultMenuStyle
    )

import String
import List
import Maybe
import Json.Decode as JD
import Json.Encode as JE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions, onInput, keyCode)

import UI.Menu as Menu

-- MODEL

type alias Model = 
  { query : String
  , menu : Menu.Model
  }

type alias Config item =
  { howMany : Int
  , search : (String -> item -> Bool)
  , toString : (item -> String)
  , input : HtmlAttributeData
  , menuConfig : Menu.Config item
  }

type alias HtmlAttributeData =
  { id : Maybe String
  , classList : List (String, Bool)
  , style : List (String, String)
  }

init : Config item -> Maybe id -> List (id, item) -> Model
init { toString } selected items =
  selected
    |> Maybe.andThen (\id -> findById id items)
    |> Maybe.map (\(_,item) -> { query = toString item, menu = Menu.empty } )
    |> Maybe.withDefault empty

empty : Model
empty =
  { query = "", menu = Menu.empty }


-- UPDATE

type Msg id
  = SetQuery String
  | BrowsePrevItem
  | BrowseNextItem
  | SelectCurrentItem
  | HideMenu
  | UpdateMenu (Menu.Msg id)
  | NoOp

update :  Config item -> Maybe id -> List (id, item) -> Msg id -> Model -> (Model, Maybe id)
update { toString, search, howMany, menuConfig } selected items msg model =
  let 
    filter query =
      searchItems search query items |> List.take howMany

    updateMenu menumsg model_ =
      Menu.update menuConfig selected (filter model_.query) menumsg model_.menu

    selectedItemText id = 
      findById id items
        |> Maybe.map (Tuple.second >> toString) 

  in
    case msg of
      UpdateMenu menumsg ->
        let 
          (newmenu, newselected) = updateMenu menumsg model
        in
          case newselected of
            Nothing ->
              ({ model | menu = newmenu }, newselected)
            Just id ->
              ( { model |
                  menu = newmenu
                , query = selectedItemText id |> Maybe.withDefault model.query
                }
              , newselected
              )

      SetQuery query ->
        let
          newmodel = { model | query = query }
          (newmenu, newselected) = updateMenu Menu.Reset newmodel
        in
          ({ newmodel | menu = newmenu }, newselected)

      BrowsePrevItem ->
        let
          (newmenu, newselected) = updateMenu Menu.SelectPrevItem model
        in
          ({ model | menu = newmenu }, newselected)

      BrowseNextItem ->
        let
          (newmenu, newselected) = updateMenu Menu.SelectNextItem model
        in
          ({ model | menu = newmenu }, newselected)

      HideMenu ->
        let
          (newmenu, newselected) = updateMenu Menu.HideMenu model
        in
          ({ model | menu = newmenu }, newselected)

      SelectCurrentItem ->
        let
          (newmenu, _) = updateMenu Menu.HideMenu model
        in
          case selected of
            Nothing -> 
              ( model, selected )
            Just id ->
              ( { model | query = selectedItemText id |> Maybe.withDefault model.query
                , menu = newmenu 
                }
              , selected
              )

      NoOp ->
        ( model, selected )


view :  Config item -> Maybe id -> List (id, item) -> Model -> Html (Msg id)
view config selected items model =
    let
        options =
            { preventDefault = True, stopPropagation = False }
        
        keyDecoder =
            (customDecoder 
                (\code ->
                    if code == 38 then
                        Ok BrowsePrevItem
                    else if code == 40 then 
                        Ok BrowseNextItem
                    else if code == 13 then
                        case selected of
                            Nothing -> Ok HideMenu
                            Just id -> Ok SelectCurrentItem
                    else if code == 27 then
                        Ok HideMenu
                    else
                        Err "not handling that key"
                )
                keyCode
            )

        relatedTargetId = 
            JD.at ["relatedTarget", "id"] JD.string 

        blurDecoder =
            JD.maybe relatedTargetId
                |> JD.map
                     ( Maybe.andThen
                        (\id -> Maybe.map 
                            (\id_ -> if id == id_ then NoOp else HideMenu) 
                            config.menuConfig.ul.id
                        )
                     )
                |> JD.map (Maybe.withDefault HideMenu)

        selectedItemText =
            selected
                |> Maybe.andThen (\id -> findById id items)
                |> Maybe.map (Tuple.second >> config.toString)
            
        inputValue =
            selectedItemText |> Maybe.withDefault model.query

        menu = 
            Menu.view config.menuConfig selected filtered model.menu
              |> Html.map UpdateMenu
            
        filtered =
            searchItems config.search model.query items |> List.take config.howMany

    in
        div []
            [ input
                [ onInput SetQuery
                , onWithOptions "keydown" options keyDecoder
                , on "blur" blurDecoder
                , value inputValue
                , Maybe.map id config.input.id |> Maybe.withDefault nullAttribute
                , classList config.input.classList
                , style config.input.style
                , autocomplete False
                , Maybe.map (attribute "aria-owns") config.menuConfig.ul.id 
                    |> Maybe.withDefault nullAttribute
                , attribute "aria-expanded" <| String.toLower <| toString model.menu.visible
                , attribute "aria-haspopup" <| String.toLower <| toString model.menu.visible
                , attribute "role" "combobox"
                , attribute "aria-autocomplete" "list"
                ]
                []
            , menu
            ]

searchItems : (String -> item -> Bool) -> String -> List (id, item) -> List (id, item)
searchItems keep str items =
  case str of
    "" -> []
    _  -> List.filter (\(_,item) -> keep str item) items

findById : id -> List (id, item) -> Maybe (id, item)
findById id items = 
  case items of
    [] -> 
      Nothing
    first :: rest ->
      if (Tuple.first first) == id then
        Just first
      else
        findById id rest


nullAttribute : Attribute msg
nullAttribute =
  property "" JE.null


defaultMenuStyle = Menu.defaultMenuStyle

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

