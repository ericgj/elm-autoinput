module Autoinput.Menu
    exposing
        ( Model
        , Config
        , init
        , empty
        , visible
        , Msg(..)
        , update
        , view
        , config
        , defaultConfig
        , menu
        , menuItem
        , menuStyle
        , menuAttributes
        , defaultMenuStyle
        )

import String
import List
import Maybe
import Result
import Json.Encode as JE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Html.Events exposing (onClick, onBlur)
import Helpers exposing (nullAttribute, mapNeverToMsg, HtmlDetails)


-- MODEL


type Model
    = Model { visible : Bool }


type Config item
    = Config
        { ul : HtmlDetails
        , li : Bool -> item -> HtmlDetails
        }


init : Bool -> Model
init visible =
    Model { visible = visible }


empty : Model
empty =
    init False



-- only needed in order to set aria- methods on parent element


visible : Model -> Bool
visible (Model model) =
    model.visible



-- UPDATE


type Msg
    = SelectPrevItem
    | SelectNextItem
    | SelectItem String
    | SelectNone
    | ShowMenu
    | HideMenu
    | ShowOrHideMenu
    | Reset
    | NoOp


update : (item -> String) -> List item -> Maybe item -> Msg -> Model -> ( Model, Maybe item )
update toId items selected msg (Model model) =
    case msg of
        SelectPrevItem ->
            let
                newselected =
                    case selected of
                        Nothing ->
                            List.head items

                        Just item ->
                            items 
                                |> selectPrev toId item
                                |> Maybe.withDefault item 
                                |> Just
            in
                ( Model { model | visible = (List.isEmpty items |> not) }
                , newselected
                )

        SelectNextItem ->
            let
                newselected =
                    case selected of
                        Nothing ->
                            List.head items

                        Just item ->
                            items
                                |> selectNext toId item
                                |> Maybe.withDefault item
                                |> Just
            in
                ( Model { model | visible = (List.isEmpty items |> not) }
                , newselected
                )

        SelectNone ->
            ( Model model, Nothing )

        SelectItem id ->
            let
                newselected =
                    findById toId id items
            in
                ( Model { model | visible = False }
                , newselected
                )

        ShowMenu ->
            ( Model { model | visible = True }, selected )

        HideMenu ->
            ( Model { model | visible = False }, selected )

        ShowOrHideMenu ->
            ( Model { model | visible = (List.isEmpty items |> not) }, selected )

        Reset ->
            ( Model { model | visible = (List.isEmpty items |> not) }, Nothing )

        NoOp ->
            ( Model model, selected )


selectPrev : (item -> String) -> item -> List item -> Maybe item
selectPrev toId item items =
    let
        getPrev cur result =
            case result of
                Err Nothing ->
                    Err (Just cur)

                Err (Just item_) ->
                    if toId item == toId item_ then
                        (Ok cur)
                    else
                        (Err (Just cur))

                Ok item_ ->
                    result
    in
        List.foldr getPrev (Err Nothing) items
            |> Result.toMaybe


selectNext : (item -> String) -> item -> List item -> Maybe item
selectNext toId item items =
    selectPrev toId item (List.reverse items)


findById : (item -> String) -> String -> List item -> Maybe item
findById toId id items =
    case items of
        [] ->
            Nothing

        first :: rest ->
            if toId first == id then
                Just first
            else
                findById toId id rest



-- VIEW


view : (item -> String) -> Config item -> Maybe item -> List item -> Model -> Html Msg
view toId (Config config) selected items (Model model) =
    if model.visible then
        viewMenu toId (Config config) selected items
    else
        text ""

viewMenu : (item -> String) -> Config item -> Maybe item -> List item -> Html Msg 
viewMenu toId (Config config) selected items =
    Html.Keyed.ul
        ( List.map (mapNeverToMsg NoOp) config.ul.attributes
              ++ [ style config.ul.style
                 , tabindex 0
                 , onBlur HideMenu
                 ]
        )
        (List.map (viewItemWithKey toId (Config config) selected) items)

viewItemWithKey : (item -> String) -> Config item -> Maybe item -> item -> ( String, Html Msg )
viewItemWithKey toId (Config config) selected item =
    let
        id = 
            toId item

        isSelected =
            selected
                |> Maybe.map (\item_ -> toId item_ == id)
                |> Maybe.withDefault False
    in
        ( id
        , viewItem (Config config) isSelected id item
        )


viewItem : Config item -> Bool -> String -> item -> Html Msg
viewItem (Config config) isSelected id item =
    let
        listItemData =
            config.li isSelected item
    in
        Html.li
            ( List.map (mapNeverToMsg NoOp) listItemData.attributes
                  ++ [ style listItemData.style
                     , onClick (SelectItem id) 
                     ]
            )
            ( List.map (Html.map (\_ -> NoOp)) listItemData.children )


-- CONFIG


config : { ul : HtmlDetails, li : Bool -> item -> HtmlDetails } -> Config item
config c =
    Config c


menuItem : (Bool -> item -> HtmlDetails) -> Config item -> Config item
menuItem fn (Config c) =
    Config { c | li = fn }


menu : HtmlDetails -> Config item -> Config item
menu details (Config c) =
    Config { c | ul = details }


menuAttributes : List (Html.Attribute Never) -> Config item -> Config item
menuAttributes attrs (Config c) =
    let
        setAttrs details =
            { details | attributes = attrs }
    in
        Config { c | ul = setAttrs c.ul }


menuStyle : List ( String, String ) -> Config item -> Config item
menuStyle styles (Config c) =
    let
        setStyle details =
            { details | style = styles }
    in
        Config { c | ul = setStyle c.ul }


defaultConfig : Config item
defaultConfig =
    config
        { ul = { attributes = [], style = minimalMenuStyle, children = [] }
        , li = minimalMenuItem
        }


minimalMenuStyle : List ( String, String )
minimalMenuStyle =
    [ ( "position", "absolute" )
    , ( "z-index", "11110" )
    , ( "list-style-type", "none" )
    , ( "cursor", "pointer" )
    ]


minimalMenuItem : Bool -> item -> HtmlDetails
minimalMenuItem selected item =
    { attributes = []
    , style = []
    , children = [ text (toString item) ]
    }


defaultMenuStyle : List ( String, String )
defaultMenuStyle =
    minimalMenuStyle
        ++ [ ( "left", "3px" )
           , ( "margin-top", "3px" )
           , ( "background", "white" )
           , ( "color", "black" )
           , ( "border", "1px solid #DDD" )
           , ( "border-radius", "3px" )
           , ( "box-shadow", "0 0 5px rgba(0,0,0,0.1)" )
           , ( "min-width", "120px" )
           ]
