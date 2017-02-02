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

{-|

This library encapsulates an _autocomplete menu_:
an input element that, as you type, narrows a list of choices visible in a menu.

Configuration and the list of choices are passed in to `update` and `view`,
but do not form part of the state managed by this library. This ensures that
your data does not go out of sync with the menu list -- and in fact you can
change the list as the user input changes, enabling common cases such as
remote querying.

Your list of choices are passed in as a `List (id, item)` &mdash; that is, a 
tuple of a unique identifier for the item, and the item itself.

There is a basic usage [example][] to look at, and more are planned.

## Prior art

This library started as a fork of the [elm-autocomplete][] "accessible" example,
and some of the view and configuration remains similar, although the underlying
implementation is different.

The configuration API is a work in progress, but owes a lot to
[elm-sortable-table][] conventions.

[example]: https://github.com/ericgj/elm-autoinput/tree/master/examples/Demo.elm

[elm-autocomplete]: https://github.com/thebritican/elm-autocomplete/tree/master/examples/src/AccessibleExample.elm

[elm-sortable-table]: https://github.com/evancz/elm-sortable-table

# View

@docs view

# Update

@docs Msg, update

# Configuration

@docs config, defaultConfig, inputAttributes, inputStyle, menu, menuItem, customConfig, Config

# State

@docs Model, State, state, empty, preselect, query, init


-}

import String
import List
import Maybe
import Json.Decode as JD
import Json.Encode as JE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions, onInput, keyCode)
import Helpers
    exposing
        ( HtmlDetails
        , HtmlAttributeDetails
        , nullAttribute
        , customDecoder
        , mapNeverToMsg
        )
import Menu as Menu


-- MODEL

{-| 

The Autoinput model consists of the input state and the menu state.
(The menu state is simply whether the menu is visible or not; otherwise, its
appearance and behavior is controlled through the Autoinput module).

-}
type Model id
    = Model
        { state : InternalState id
        , menu : Menu.Model
        }


{-|

Note the internal _input_ state used by this module has more information than
the externally facing _selection_ State. It distinguishes between pre-selection
and selection, and retains the query string even after a selection is made.

-}
type InternalState id
    = Initial
    | Preselecting id
    | Querying String
    | Selecting String id


{-| 

Selection state. Note to get the current state from an Autoinput model, use 
`state model`.

-}
type State id
    = NoInput
    | Entered String
    | Selected id

{-|  Configuration passed to both `view` and `update`.
-}
type Config item
    = Config
        { howMany : Int
        , search : String -> item -> Bool
        , toString : item -> String
        , input : HtmlAttributeDetails
        , menuId : String
        , menuConfig : Menu.Config item
        }


{-| Construct an empty model (no input, hidden menu).
-}
empty : Model id
empty =
    initInternal Initial


{-| Construct a model with an initial selected value.
-}
preselect : id -> Model id
preselect id =
    initInternal (Preselecting id)


{-| Construct a model with an initial query value.
-}
query : String -> Model id
query q =
    initInternal (Querying q)


{-| Construct a model from given State.
-}
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


{-| Extract internal state of model to State.
-}
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


{-| 

Note that the Msg type specifies the type of the item `id`. So for instance
if the list you pass in to `view` and `update` is `List (Int, String)`, then
your wrapped Msg will be `UpdateAutoinput (Autoinput.Msg Int)`.  See `update`
example.

-}
type Msg id
    = SetQuery String
    | BrowsePrevItem
    | BrowseNextItem
    | HideMenu
    | UpdateMenu (Menu.Msg id)
    | NoOp


{-|

Handle input. Your application should nest Autoinput messages in the typical
way using `Html.map`. Note that Autoinput Msg type takes an `id` parameter: the
ID type of items in your list.

Here is an example:

    -- view

    div []
        [ label [] [text "Select a thing"]
        , Autoinput.view config things model |> Html.map UpdateAutoInput
        ]

    -- update

    type Msg
        -- ...
        | UpdateAutoInput (Autoinput.Msg ThingId)
        -- ...

    update msg model =
        case msg of
            UpdateAutoInput automsg ->
                Autoinput.update config things automsg model


Note that you pass two pieces of context in to `update`:

  - The `Config`,  and
  - The full list of items, as tuples of `(id, item)`.

-}
update : Config item -> List ( id, item ) -> Msg id -> Model id -> Model id
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
                    ( newMenu, selected ) =
                        updateMenu menumsg model.menu

                    newState =
                        selected
                            |> Maybe.map (setSelecting model.state)
                            |> Maybe.withDefault model.state
                in
                    Model { model | state = newState, menu = newMenu }

            SetQuery query ->
                let
                    ( newMenu, _ ) =
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


{-|

Render the input element and associated menu div.

Note that the same as for `update`, you pass in two pieces of context:

  - The `Config`,  and
  - The full list of items, as tuples of `(id, item)`.

-}
view : Config item -> List ( id, item ) -> Model id -> Html (Msg id)
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
                    (Maybe.map
                        (\id ->
                            if id == config.menuId then
                                NoOp
                            else
                                HideMenu
                        )
                    )
                |> JD.map (Maybe.withDefault HideMenu)

        relatedTargetId =
            JD.at [ "relatedTarget", "id" ] JD.string

        filteredItems =
            menuItems config.search config.howMany items model.state

        val =
            inputValue config.toString items model.state
    in
        div []
            [ input
                (List.map (mapNeverToMsg NoOp) config.input.attributes
                    ++ [ style config.input.style ]
                    ++ [ onInput SetQuery
                       , onWithOptions "keydown" preventDefault keyDecoder
                       , on "blur" hideMenuUnlessRelatedTarget
                       , value val
                       , autocomplete False
                       , attribute "aria-owns" config.menuId
                       , attribute "aria-expanded" <|
                            String.toLower <|
                                toString <|
                                    Menu.visible model.menu
                       , attribute "aria-haspopup" <|
                            String.toLower <|
                                toString <|
                                    Menu.visible model.menu
                       , attribute "role" "combobox"
                       , attribute "aria-autocomplete" "list"
                       ]
                )
                []
            , viewMenu menuConfig filteredItems (Model model)
            ]


viewMenu : Menu.Config item -> List ( id, item ) -> Model id -> Html (Msg id)
viewMenu menuConfig items (Model model) =
    Menu.view menuConfig (Model model |> toMaybe) items model.menu
        |> Html.map UpdateMenu



-- HELPERS


searchItems : (String -> item -> Bool) -> String -> List ( id, item ) -> List ( id, item )
searchItems keep str items =
    case str of
        "" ->
            []

        _ ->
            List.filter (\( _, item ) -> keep str item) items


findById : id -> List ( id, item ) -> Maybe ( id, item )
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
            Selecting "" id

        -- should not be able to get here
        Querying query ->
            Selecting query id

        Selecting query _ ->
            Selecting query id


selectedItemText : (item -> String) -> List ( id, item ) -> id -> Maybe String
selectedItemText toString items id =
    findById id items
        |> Maybe.map (Tuple.second >> toString)


inputValue : (item -> String) -> List ( id, item ) -> InternalState id -> String
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


{-|

Note subtle difference between `queryValue` and `inputValue` when Selecting.

`inputValue` is the value of the input element, and should be the text of
the looked-up selected item. This is used in the `view`.

`queryValue` is the last entered query text by the user, which determines
the menu choices. This is used in `update`.

In a nutshell, this is what makes state management tricky in autocomplete.

-}
queryValue : (item -> String) -> List ( id, item ) -> InternalState id -> String
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


menuItems : (String -> item -> Bool) -> Int -> List ( id, item ) -> InternalState id -> List ( id, item )
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


{-| Create the `config` to pass into `update` and `view`. Everything we need in
order to filter and display the menu in response to user input.

For example, say we want to build a menu of `Person`s, searching either their
name or their email address for any match of the entered text, but only display
their names in the menu. We might have a `Config` like this:

    import Autoinput

    config : Autoinput.Config Person
    config =
        Autoinput.config
            { howMany = 5
            , search = searchNameAndEmail
            , toString = .name
            , menuId = "person-menu"
            , menuItemStyle = menuItem
            }

    searchNameAndEmail : String -> Person -> Bool
    searchNameAndEmail query person =
        [person.name, person.email]
            |> List.map String.toLower
            |> List.any (String.contains (String.toLower query))

    menuItem : Bool -> List (String, String)
    menuItem selected =
        if selected then
            [ ("background-color", "light-yellow") ]
        else
            [ ]

You provide the following information:

  - `howMany` &mdash; maximum number of matched items to display in menu
  - `search` &mdash; search function `String -> item -> Bool`
  - `toString` &mdash; how your items should be displayed
  - `menuId` &mdash; the DOM id of the "menu" element
  - `menuItemStyle` &mdash; how a menu item should be styled depending on if it
     is selected or not.

More fine-grained configuration is possible through `defaultConfig` and
`customConfig`.

-}
config :
    { howMany : Int
    , search : String -> item -> Bool
    , toString : item -> String
    , menuId : String
    , menuItemStyle : Bool -> List ( String, String )
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


{-| TODO
-}
defaultConfig :
    { howMany : Int
    , search : String -> item -> Bool
    , toString : item -> String
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


{-| TODO
-}
inputAttributes : List (Html.Attribute Never) -> Config item -> Config item
inputAttributes attrs (Config c) =
    let
        setAttrs details =
            { details | attributes = attrs }
    in
        Config { c | input = setAttrs c.input }


{-| TODO
-}
inputStyle : List ( String, String ) -> Config item -> Config item
inputStyle styles (Config c) =
    let
        setStyle details =
            { details | style = styles }
    in
        Config { c | input = setStyle c.input }


{-| TODO
-}
menu : Menu.Config item -> Config item -> Config item
menu m (Config c) =
    Config { c | menuConfig = m }


{-| TODO
-}
menuItem : (Bool -> item -> HtmlDetails) -> Config item -> Config item
menuItem fn (Config c) =
    Config { c | menuConfig = Menu.menuItem fn c.menuConfig }


{-| TODO
-}
menuStyle : List ( String, String ) -> Config item -> Config item
menuStyle styles (Config c) =
    Config { c | menuConfig = Menu.menuStyle styles c.menuConfig }


{-| TODO
-}
menuAttributes : List (Html.Attribute Never) -> Config item -> Config item
menuAttributes attrs (Config c) =
    Config { c | menuConfig = Menu.menuAttributes attrs c.menuConfig }


defaultInput : HtmlAttributeDetails
defaultInput =
    { attributes = [], style = [] }


{-| TODO
-}
customConfig :
    { howMany : Int
    , search : String -> item -> Bool
    , toString : item -> String
    , input : HtmlAttributeDetails
    , menuId : String
    , menuConfig : Menu.Config item
    }
    -> Config item
customConfig c =
    Config c
