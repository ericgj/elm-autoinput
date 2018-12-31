# elm-autoinput

```
elm package install ericgj/elm-autoinput
```

A simple autocomplete menu in Elm.

## Usage

### Types

First you need to add Autoinput types to your messages and your model

```elm
import Autoinput

type Msg
    = UpdateAutoInput (Autoinput.Msg Int)

type alias Model =
    Autoinput.Model Int
```

### Initialization

Initializes your model.

```elm
initModel: Model
initModel =
    Autoinput.preselect 5
```

### Config

You will have to create a `Config` for your component.

**Note** Your `Config` should _never_ be held in your model. It should only appear in your `view` code.

```elm
config : Autoinput.Config Int Item
config =
    Autoinput.defaultConfig
        { howMany = 10
        , search = searchConfig
        , toString = .name
        , idToString = String.fromInt
        , menuId = "item-menu"
        }

searchConfig : String -> Item -> Bool
searchConfig q item =
    item.name
        |> String.toLower
        |> String.contains (String.toLower q)
```

The `config` function needs functions to match the search items, convert item and associated id to string.
You can also specify styles for your menu and menu items.

### View

You will need to render the component with the config you have defined and a list of items you need
to filter from.

```elm
view : Model -> Html Msg
view model =
    div [] [ Autoinput.view config items model |> Html.map UpdateAutoInput ]
```

### Update

Plug in the update function into your update function to handle the events properly.

```elm
update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateAutoInput submsg ->
            Autoinput.update config items submsg model
```


## Examples

To run the examples go to the `examples` directory, install dependencies and run `elm reactor`:

```
> cd examples/
> elm reactor
```
