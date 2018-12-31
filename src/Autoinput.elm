module Autoinput exposing
    ( view
    , Msg, update
    , empty, preselect, query, init
    , Config, config, defaultConfig, inputAttributes, inputStyle, menu, menuItem, customConfig
    , State(..), Model, state
    )

{-| This library encapsulates an _autocomplete menu_:
an input element that, as you type, narrows a list of choices visible in a menu.

Configuration and the list of choices are passed in to `update` and `view`,
but do not form part of the state managed by this library. This ensures that
your data does not go out of sync with the menu list &mdash; and in fact you can
change the list as the user input changes, enabling common cases such as
remote querying.

Your list of choices are passed in as a `List (id, item)` &mdash; that is, a
tuple of a unique identifier for the item, and the item itself.

There is a basic usage [example] to look at, and more are planned.


## Prior art

This library started as a fork of the [elm-autocomplete] "accessible" example,
and some of the view and configuration remains similar, although the underlying
implementation is different.

PLEASE NOTE:

  - The configuration API is a work in progress, but owes a lot to
    [elm-sortable-table] conventions.
  - It works, but needs a bit of work still to get to a comfortable UX.

[example]: https://github.com/ericgj/elm-autoinput/tree/master/examples/Demo.elm

[elm-autocomplete]: https://github.com/thebritican/elm-autocomplete/tree/master/examples/src/AccessibleExample.elm

[elm-sortable-table]: https://github.com/evancz/elm-sortable-table


# View

@docs view


# Update

@docs Msg, update


# Constructors

@docs empty, preselect, query, init


# Configuration

@docs Config, config, defaultConfig, inputAttributes, inputStyle, menu, menuItem, customConfig


# State

@docs State, Model, state

-}

import Helpers
    exposing
        ( HtmlAttributeDetails
        , HtmlDetails
        , boolToString
        , customDecoder
        , mapNeverToMsg
        , nullAttribute
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onInput, preventDefaultOn)
import Json.Decode as JD
import Json.Encode as JE
import List
import Maybe
import Menu as Menu
import String



-- MODEL


{-| The internal Autoinput model consists of the input state and the menu state.

(Implementation note: the menu state is simply whether the menu is visible or
not; otherwise, its appearance and behavior is controlled through the Autoinput
module).

-}
type Model id
    = Model
        { state : InternalState id
        , menu : Menu.Model
        }


{-| Note the internal _input_ state used by this module has more information than
the externally facing _selection_ State. It distinguishes between pre-selection
and selection, and retains the query string even after a selection is made.
-}
type InternalState id
    = Initial
    | Preselecting id
    | Querying String
    | Selecting String id


{-| Selection state. Note to get the current selection state from an Autoinput model,
use `Autoinput.state model`.

  - `NoInput`: user hasn't entered anything yet or input box is otherwise empty.
  - `Entered String`: user has entered a query string, but has not made a
    selection from the menu.
  - `Selected id`: user has made a selection from the menu, and here is its
    `id`.

-}
type State id
    = NoInput
    | Entered String
    | Selected id


{-| Configuration passed to both `view` and `update`. See below for how to specify
`Config`.
-}
type Config id item
    = Config
        { howMany : Int
        , search : String -> item -> Bool
        , toString : item -> String
        , input : HtmlAttributeDetails
        , menuId : String
        , menuConfig : Menu.Config id item
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


{-| Construct a model from given State. Useful if you are passing in a previously
stored state, e.g. loaded from local storage or server.
-}
init : State id -> Model id
init state_ =
    case state_ of
        NoInput ->
            initInternal Initial

        Selected id ->
            initInternal (Preselecting id)

        Entered q ->
            initInternal (Querying q)


initInternal : InternalState id -> Model id
initInternal state_ =
    Model { state = state_, menu = Menu.empty }


{-| Get external selection state. Useful for example if you want to persist
selection state and load it in (via `init`) at another time.
-}
state : Model id -> State id
state (Model model) =
    case model.state of
        Initial ->
            NoInput

        Preselecting id ->
            Selected id

        Querying query_ ->
            Entered query_

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


{-| Internal `Msg` type used by Autoinput. Your update function should wrap msgs
of this type, with the type of the item `id` as a parameter.

So for instance if the list you pass in to `view` and `update` is
`List (Int, String)`, then your wrapped Msg will be something like
`UpdateAutoinput (Autoinput.Msg Int)`. See the `update` example below.

-}
type Msg id
    = SetQuery String
    | BrowsePrevItem
    | BrowseNextItem
    | HideMenu
    | UpdateMenu (Menu.Msg id)
    | NoOp


{-| Handle input. Your application should nest Autoinput messages using `Html.map`.
Note that the `Msg` type takes an `id` parameter: the ID type of items in
your list (the first element of the tuple).

Note that you pass two pieces of context in to `update`:

  - The `Config`, and
  - The full list of items, as tuples of `(id, item)`.

Here is an example:

    -- view

    div []
        [ label [] [text "Select a thing"]
        , Autoinput.view config things model |> Html.map UpdateAutoInput
        ]

    -- update

    type Msg
        = UpdateAutoInput (Autoinput.Msg ThingId)
        -- ...

    update msg model =
        case msg of
            UpdateAutoInput automsg ->
                Autoinput.update config things automsg model

-}
update : Config id item -> List ( id, item ) -> Msg id -> Model id -> Model id
update (Config config_) items msg (Model model) =
    let
        query_ =
            queryValue config_.toString items model.state

        filteredItems =
            searchItems config_.search query_ items |> List.take config_.howMany

        updateMenu menumsg menu_ =
            Menu.update filteredItems (Model model |> toMaybe) menumsg menu_
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

        SetQuery newQuery ->
            let
                ( newMenu, _ ) =
                    updateMenu Menu.Reset model.menu

                newState =
                    if newQuery == "" then
                        Initial

                    else
                        Querying newQuery
            in
            Model { model | state = newState, menu = newMenu }

        BrowsePrevItem ->
            update (Config config_) items (UpdateMenu Menu.SelectPrevItem) (Model model)

        BrowseNextItem ->
            update (Config config_) items (UpdateMenu Menu.SelectNextItem) (Model model)

        HideMenu ->
            update (Config config_) items (UpdateMenu Menu.HideMenu) (Model model)

        NoOp ->
            Model model


{-| Render the input field and associated menu.

Note that the same as for `update`, you pass in two pieces of context:

  - The `Config` (see below), and
  - The full list of items, as tuples of `(id, item)`.

-}
view : Config id item -> List ( id, item ) -> Model id -> Html (Msg id)
view (Config config_) items (Model model) =
    let
        menuConfig =
            Menu.menuAttributes [ Html.Attributes.id config_.menuId ] config_.menuConfig

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
                            if id == config_.menuId then
                                NoOp

                            else
                                HideMenu
                        )
                    )
                |> JD.map (Maybe.withDefault HideMenu)

        relatedTargetId =
            JD.at [ "relatedTarget", "id" ] JD.string

        filteredItems =
            menuItems config_.search config_.howMany items model.state

        val =
            inputValue config_.toString items model.state
    in
    div []
        [ input
            (List.map (mapNeverToMsg NoOp) config_.input.attributes
                ++ List.map (\( attr, attrVal ) -> style attr attrVal) config_.input.style
                ++ [ onInput SetQuery
                   , preventDefaultOn "keydown" keyDecoder
                   , on "blur" hideMenuUnlessRelatedTarget
                   , value val
                   , autocomplete False
                   , attribute "aria-owns" config_.menuId
                   , attribute "aria-expanded" <|
                        String.toLower <|
                            boolToString <|
                                Menu.visible model.menu
                   , attribute "aria-haspopup" <|
                        String.toLower <|
                            boolToString <|
                                Menu.visible model.menu
                   , attribute "role" "combobox"
                   , attribute "aria-autocomplete" "list"
                   ]
            )
            []
        , viewMenu menuConfig filteredItems (Model model)
        ]


viewMenu : Menu.Config id item -> List ( id, item ) -> Model id -> Html (Msg id)
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
            if Tuple.first first == id then
                Just first

            else
                findById id rest


setSelecting : InternalState id -> id -> InternalState id
setSelecting state_ id =
    case state_ of
        Initial ->
            Selecting "" id

        Preselecting _ ->
            Selecting "" id

        -- should not be able to get here
        Querying query_ ->
            Selecting query_ id

        Selecting query_ _ ->
            Selecting query_ id


selectedItemText : (item -> String) -> List ( id, item ) -> id -> Maybe String
selectedItemText toString items id =
    findById id items
        |> Maybe.map (Tuple.second >> toString)


inputValue : (item -> String) -> List ( id, item ) -> InternalState id -> String
inputValue toString items state_ =
    case state_ of
        Initial ->
            ""

        Preselecting id ->
            selectedItemText toString items id
                |> Maybe.withDefault ""

        Querying query_ ->
            query_

        Selecting _ id ->
            selectedItemText toString items id
                |> Maybe.withDefault ""


{-| Note subtle difference between `queryValue` and `inputValue` when Selecting.

`inputValue` is the value of the input element, and should be the text of
the looked-up selected item. This is used in the `view`.

`queryValue` is the last entered query text by the user, which determines
the menu choices. This is used in `update`.

In a nutshell, this is what makes state management tricky in autocomplete.

-}
queryValue : (item -> String) -> List ( id, item ) -> InternalState id -> String
queryValue toString items state_ =
    case state_ of
        Initial ->
            ""

        Preselecting id ->
            selectedItemText toString items id
                |> Maybe.withDefault ""

        Querying query_ ->
            query_

        Selecting query_ id ->
            query_


menuItems : (String -> item -> Bool) -> Int -> List ( id, item ) -> InternalState id -> List ( id, item )
menuItems search howMany items state_ =
    case state_ of
        Initial ->
            []

        Preselecting id ->
            []

        Querying query_ ->
            searchItems search query_ items |> List.take howMany

        Selecting query_ _ ->
            searchItems search query_ items |> List.take howMany



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
        [ person.name, person.email ]
            |> List.map String.toLower
            |> List.any (String.contains (String.toLower query))

    menuItem : Bool -> List ( String, String )
    menuItem selected =
        if selected then
            [ ( "background-color", "light-yellow" ) ]

        else
            []

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
    , idToString : id -> String
    , menuId : String
    , menuItemStyle : Bool -> List ( String, String )
    }
    -> Config id item
config { howMany, search, toString, idToString, menuId, menuItemStyle } =
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
        , menuConfig = Menu.defaultConfig idToString toString |> Menu.menuItem menuItem_
        }


{-| The same as `config`, but with a default configuration for the menu items.
-}
defaultConfig :
    { howMany : Int
    , search : String -> item -> Bool
    , toString : item -> String
    , idToString : id -> String
    , menuId : String
    }
    -> Config id item
defaultConfig { howMany, search, toString, idToString, menuId } =
    Config
        { howMany = howMany
        , search = search
        , toString = toString
        , input = defaultInput
        , menuId = menuId
        , menuConfig = Menu.defaultConfig idToString toString
        }


{-| Set attributes of the input field.

    config =
        defaultConfig
            |> inputAttributes [ class "autoinput" ]

-}
inputAttributes : List (Html.Attribute Never) -> Config id item -> Config id item
inputAttributes attrs (Config c) =
    let
        setAttrs details =
            { details | attributes = attrs }
    in
    Config { c | input = setAttrs c.input }


{-| Set style of the input field.

    config =
        defaultConfig
            |> inputStyle [ ( "border-radius", "5px" ) ]

-}
inputStyle : List ( String, String ) -> Config id item -> Config id item
inputStyle styles (Config c) =
    let
        setStyle details =
            { details | style = styles }
    in
    Config { c | input = setStyle c.input }


{-| Set menu configuration directly. Usually it's more straightforward to use
`menuItem` (for individual menu items), or `menuStyle` and `menuAttributes`
(for the menu itself).
-}
menu : Menu.Config id item -> Config id item -> Config id item
menu m (Config c) =
    Config { c | menuConfig = m }


{-| Set menu item attributes, style, and children (view), as a function of

  - whether the current item is selected or not, and
  - the current item.

```
let
    menuItemPieces selected item =
        { attributes = [ class "autoinput-menu-item" ]
        , style = if selected then highlight else []
        , children =
          [ div []
            [ span ( if item.starred then [ class = "star" ] else [] ) []
            , span [] [ text item.name ]
            ]
          ]
        }
in
    defaultConfig
       |> menuItem menuItemPieces
```

-}
menuItem : (Bool -> item -> HtmlDetails) -> Config id item -> Config id item
menuItem fn (Config c) =
    Config { c | menuConfig = Menu.menuItem fn c.menuConfig }


{-| Set menu style.

    defaultConfig
        |> menuStyle [ ( "background-color", "#EEE" ) ]

-}
menuStyle : List ( String, String ) -> Config id item -> Config id item
menuStyle styles (Config c) =
    Config { c | menuConfig = Menu.menuStyle styles c.menuConfig }


{-| Set menu attributes.

    defaultConfig
        |> menuAttributes [ class "autocomplete-menu" ]

-}
menuAttributes : List (Html.Attribute Never) -> Config id item -> Config id item
menuAttributes attrs (Config c) =
    Config { c | menuConfig = Menu.menuAttributes attrs c.menuConfig }


defaultInput : HtmlAttributeDetails
defaultInput =
    { attributes = [], style = [] }


{-| The most generic configuration. Use at your own risk, it may change in future
versions.
-}
customConfig :
    { howMany : Int
    , search : String -> item -> Bool
    , toString : item -> String
    , input : HtmlAttributeDetails
    , menuId : String
    , menuConfig : Menu.Config id item
    }
    -> Config id item
customConfig c =
    Config c
