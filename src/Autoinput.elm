module Autoinput 
  exposing
    ( Model
    , State
    , Config
    , Msg
    , preselect
    , empty
    , query
    , init
    , state
    , update
    , view
    , config
    , defaultConfig
    , customConfig
    , inputAttributes
    , inputStyle
    , menu
    , menuItem
    )

import String
import List
import Maybe
import Json.Decode as JD
import Json.Encode as JE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions, onInput, keyCode)

import Helpers exposing 
  ( HtmlDetails
  , HtmlAttributeDetails
  , nullAttribute
  , customDecoder
  , mapNeverToMsg
  )
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

type Config item =
  Config
    { howMany : Int
    , search : (String -> item -> Bool)
    , toString : (item -> String)
    , input : HtmlAttributeDetails
    , menuId : String
    , menuConfig : Menu.Config item 
    }

empty : Model id
empty =
  initInternal Initial

preselect : id -> Model id
preselect id =
  initInternal (Preselecting id)

query : String -> Model id
query q =
  initInternal (Querying q)

init : State id -> Model id
init state =
  case state of
    NoInput ->
      initInternal Initial
    Selected id ->
      initInternal (Preselecting id)
    Entered q ->
      initInternal (Querying q)

initInternal : InternalState id -> Model id
initInternal state =
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
update (Config config) items msg (Model model) =
  let 
    query =
      queryValue config.toString items model.state

    filteredItems =
      searchItems config.search query items |> List.take config.howMany

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
        update (Config config) items (UpdateMenu Menu.SelectPrevItem) (Model model)

      BrowseNextItem ->
        update (Config config) items (UpdateMenu Menu.SelectNextItem) (Model model)

      HideMenu ->
        update (Config config) items (UpdateMenu Menu.HideMenu) (Model model)

      NoOp ->
        (Model model)


view :  Config item -> List (id, item) -> Model id -> Html (Msg id)
view (Config config) items (Model model) =
    let
        menuConfig =
            Menu.menuAttributes [ Html.Attributes.id config.menuId ] config.menuConfig

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
                          if id == config.menuId then 
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

        val =
            inputValue config.toString items model.state

    in
        div []
            [ input
                ( List.map (mapNeverToMsg NoOp) config.input.attributes ++ 
                  [ style config.input.style ] ++
                  [ onInput SetQuery
                  , onWithOptions "keydown" preventDefault keyDecoder
                  , on "blur" hideMenuUnlessRelatedTarget
                  , value val
                  , autocomplete False
                  , attribute "aria-owns" config.menuId 
                  , attribute "aria-expanded" 
                      <| String.toLower <| toString <| Menu.visible model.menu
                  , attribute "aria-haspopup" 
                      <| String.toLower <| toString <| Menu.visible model.menu
                  , attribute "role" "combobox"
                  , attribute "aria-autocomplete" "list"
                  ]
                )
                []
            , viewMenu menuConfig filteredItems (Model model)
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

    Selecting query id ->
      selectedItemText toString items id  
        |> Maybe.withDefault ""


{-| Note subtle difference between `queryValue` and `inputValue` when Selecting. 
    
    `inputValue` is the value of the input element, and should be the text of
    the looked-up selected item. This is used in the `view`.
    
    `queryValue` is the last entered query text by the user, which determines
    the menu choices. This is used in `update`.

    In a nutshell, this is what makes state management tricky in autocomplete.

-}
queryValue : (item -> String) -> List (id,item) -> InternalState id -> String
queryValue toString items state =
  case state of
    Initial ->
      ""

    Preselecting id ->
      selectedItemText toString items id
        |> Maybe.withDefault ""

    Querying query ->
      query

    Selecting query id ->
      query


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


-- CONFIG

config :
    { howMany : Int
    , search : (String -> item -> Bool)
    , toString : (item -> String)
    , menuId : String
    , menuItemStyle : (Bool -> List (String, String))
    } 
    -> Config item 
config { howMany, search, toString, menuId, menuItemStyle } =
    let 
        menuItem_ selected item =
            { attributes = []
            , style = menuItemStyle selected
            , children = [ text (toString item) ]
            }
    in
        Config
            { howMany = howMany
            , search = search
            , toString = toString
            , input = defaultInput
            , menuId = menuId
            , menuConfig = Menu.defaultConfig |> Menu.menuItem menuItem_
            }

defaultConfig :
    { howMany : Int
    , search : (String -> item -> Bool)
    , toString : (item -> String)
    , menuId : String
    }
    -> Config item
defaultConfig { howMany, search, toString, menuId } =
    Config 
        { howMany = howMany
        , search = search
        , toString = toString
        , input = defaultInput
        , menuId = menuId
        , menuConfig = Menu.defaultConfig
        }

inputAttributes : List (Html.Attribute Never) -> Config item -> Config item
inputAttributes attrs (Config c) =
    let
        setAttrs details = { details | attributes = attrs }
    in
      Config { c | input = setAttrs c.input }

inputStyle : List (String,String) -> Config item -> Config item
inputStyle styles (Config c) =
    let
        setStyle details = { details | style = styles }
    in
      Config { c | input = setStyle c.input }

menu : Menu.Config item -> Config item -> Config item
menu m (Config c) =
    Config { c | menuConfig = m }


menuItem : ( Bool -> item -> HtmlDetails ) -> Config item -> Config item
menuItem fn (Config c) =
    Config { c | menuConfig = Menu.menuItem fn c.menuConfig }

menuStyle : List (String,String) -> Config item -> Config item
menuStyle styles (Config c) =
    Config { c | menuConfig = Menu.menuStyle styles c.menuConfig }

menuAttributes : List (Html.Attribute Never) -> Config item -> Config item
menuAttributes attrs (Config c) =
    Config { c | menuConfig = Menu.menuAttributes attrs c.menuConfig }


defaultInput : HtmlAttributeDetails
defaultInput =
  { attributes = [], style = [] }


customConfig : 
    { howMany : Int
    , search : (String -> item -> Bool)
    , toString : (item -> String)
    , input : HtmlAttributeDetails
    , menuId : String
    , menuConfig : Menu.Config item 
    }
    -> Config item
customConfig c =
    Config c



