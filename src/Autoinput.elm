module Autoinput 
  exposing
    ( Model
    , State
    , Config
    , Msg
    , init
    , preselect
    , empty
    , query
    , state
    , update
    , view
    )

import String
import List
import Maybe
import Json.Decode as JD
import Json.Encode as JE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions, onInput, keyCode)

import Helpers exposing (nullAttribute, customDecoder, mapNeverToMsg)
import Menu as Menu


-- MODEL

type Model id =
  Model
    { state : InternalState id
    , menu : Menu.Model
    }

type InternalState id
  = Initial
  | Preselecting id
  | Querying String
  | Selecting String id

type State id
  = NoInput
  | Entered String
  | Selected id

type alias Config item =
  { howMany : Int
  , search : (String -> item -> Bool)
  , toString : (item -> String)
  , input : HtmlAttributeDetails
  , menuConfig : Menu.Config item 
  }

type alias HtmlAttributeDetails =
  { attributes : List (Attribute Never)
  , style : List (String, String)
  }

empty : Model id
empty =
  init Initial

preselect : id -> Model id
preselect id =
  init (Preselecting id)

query : String -> Model id
query q =
  init (Querying q)

init : InternalState id -> Model id
init state =
  Model { state = state, menu = Menu.empty }

state : Model id -> State id
state (Model model) =
  case model.state of
    Initial ->
      NoInput
    Preselecting id ->
      Selected id
    Querying query ->
      Entered query
    Selecting _ id ->
      Selected id
    
toMaybe : Model id -> Maybe id
toMaybe (Model model) = 
  case model.state of
    Preselecting id ->
      Just id
    Selecting _ id ->
      Just id
    _ ->
      Nothing


-- UPDATE

type Msg id
  = SetQuery String
  | BrowsePrevItem
  | BrowseNextItem
  | HideMenu
  | UpdateMenu (Menu.Msg id)
  | NoOp


update :  Config item -> List (id, item) -> Msg id -> Model id -> Model id
update config items msg (Model model) =
  let 
    queryValue =
      inputValue config.toString items model.state

    filteredItems =
      searchItems config.search queryValue items |> List.take config.howMany

    updateMenu menumsg menu =
      Menu.update filteredItems (Model model |> toMaybe) menumsg menu

  in
    case msg of
      UpdateMenu menumsg ->
        let 
          (newMenu, selected) = 
            updateMenu menumsg model.menu
          newState =
            selected
              |> Maybe.map (setSelecting model.state)
              |> Maybe.withDefault model.state
        in
          Model { model | state = newState, menu = newMenu }

      SetQuery query ->
        let
          (newMenu, _) = 
            updateMenu Menu.Reset model.menu
          newState = 
            Querying query
        in
          Model { model | state = newState, menu = newMenu }

      BrowsePrevItem ->
        update config items (UpdateMenu Menu.SelectPrevItem) (Model model)

      BrowseNextItem ->
        update config items (UpdateMenu Menu.SelectNextItem) (Model model)

      HideMenu ->
        update config items (UpdateMenu Menu.HideMenu) (Model model)

      NoOp ->
        (Model model)


view :  Config item -> List (id, item) -> Model id -> Html (Msg id)
view config items (Model model) =
    let
        preventDefault =
            { preventDefault = True, stopPropagation = False }
        
        keyDecoder = 
          customDecoder keyResult keyCode

        keyResult code =
            if code == 38 then
                Ok BrowsePrevItem
            else if code == 40 then 
                Ok BrowseNextItem
            else if code == 13 then
                Ok HideMenu
            else if code == 27 then
                Ok HideMenu
            else
                Err "not handling that key"

        hideMenuUnlessRelatedTarget =
            JD.maybe relatedTargetId
                |> JD.map
                     ( Maybe.map
                        (\id -> 
                          if id == config.menuConfig.id then 
                            NoOp 
                          else 
                            HideMenu
                        ) 
                     )
                |> JD.map (Maybe.withDefault HideMenu)


        relatedTargetId = 
            JD.at ["relatedTarget", "id"] JD.string 

        filteredItems =
            menuItems config.search config.howMany items model.state

        queryValue =
            inputValue config.toString items model.state

    in
        div []
            [ input
                ( List.map (mapNeverToMsg NoOp) config.input.attributes ++ 
                  [ style config.input.style ] ++
                  [ onInput SetQuery
                  , onWithOptions "keydown" preventDefault keyDecoder
                  , on "blur" hideMenuUnlessRelatedTarget
                  , value queryValue
                  , autocomplete False
                  , attribute "aria-owns" config.menuConfig.id 
                  , attribute "aria-expanded" 
                      <| String.toLower <| toString <| Menu.visible model.menu
                  , attribute "aria-haspopup" 
                      <| String.toLower <| toString <| Menu.visible model.menu
                  , attribute "role" "combobox"
                  , attribute "aria-autocomplete" "list"
                  ]
                )
                []
            , viewMenu config.menuConfig filteredItems (Model model)
            ]


viewMenu : Menu.Config item -> List (id, item) -> Model id -> Html (Msg id)
viewMenu menuConfig items (Model model) =
    Menu.view menuConfig (Model model |> toMaybe) items model.menu
        |> Html.map UpdateMenu


-- HELPERS

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


setSelecting : InternalState id -> id -> InternalState id
setSelecting state id =
  case state of
    Initial ->
      Selecting "" id
    Preselecting _ ->
      Selecting "" id     -- should not be able to get here
    Querying query ->
      Selecting query id
    Selecting query _ ->
      Selecting query id  

selectedItemText : (item -> String) -> List (id,item) -> id -> Maybe String
selectedItemText toString items id = 
  findById id items
    |> Maybe.map (Tuple.second >> toString) 


inputValue : (item -> String) -> List (id,item) -> InternalState id -> String
inputValue toString items state =
  case state of
    Initial ->
      ""

    Preselecting id ->
      selectedItemText toString items id
        |> Maybe.withDefault ""

    Querying query ->
      query

    Selecting _ id ->
      selectedItemText toString items id  
        |> Maybe.withDefault ""


menuItems : (String -> item -> Bool) -> Int -> List (id,item) -> InternalState id -> List (id,item)
menuItems search howMany items state =
  case state of
    Initial ->
      []
    
    Preselecting id ->
      []

    Querying query ->
      searchItems search query items |> List.take howMany
    
    Selecting query _ ->
      searchItems search query items |> List.take howMany



