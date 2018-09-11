module Menu exposing
    ( Config
    , Model
    , Msg(..)
    , config
    , defaultConfig
    , defaultMenuStyle
    , empty
    , init
    , menu
    , menuAttributes
    , menuItem
    , menuStyle
    , update
    , view
    , visible
    )

import Helpers exposing (HtmlDetails, mapNeverToMsg, nullAttribute)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick)
import Html.Keyed
import Json.Encode as JE
import List
import Maybe
import Result
import String



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


type Msg id
    = SelectPrevItem
    | SelectNextItem
    | SelectItem id
    | SelectNone
    | ShowMenu
    | HideMenu
    | ShowOrHideMenu
    | Reset
    | NoOp


update : List ( id, item ) -> Maybe id -> Msg id -> Model -> ( Model, Maybe id )
update items selected msg (Model model) =
    case msg of
        SelectPrevItem ->
            let
                newselected =
                    case selected of
                        Nothing ->
                            List.head items |> Maybe.map Tuple.first

                        Just id ->
                            List.map Tuple.first items |> selectPrev id |> Maybe.withDefault id |> Just
            in
            ( Model { model | visible = List.isEmpty items |> not }
            , newselected
            )

        SelectNextItem ->
            let
                newselected =
                    case selected of
                        Nothing ->
                            List.head items |> Maybe.map Tuple.first

                        Just id ->
                            List.map Tuple.first items
                                |> selectNext id
                                |> Maybe.withDefault id
                                |> Just
            in
            ( Model { model | visible = List.isEmpty items |> not }
            , newselected
            )

        SelectNone ->
            ( Model model, Nothing )

        SelectItem id ->
            let
                newselected =
                    findById id items |> Maybe.map Tuple.first
            in
            ( Model { model | visible = False }
            , newselected
            )

        ShowMenu ->
            ( Model { model | visible = True }, selected )

        HideMenu ->
            ( Model { model | visible = False }, selected )

        ShowOrHideMenu ->
            ( Model { model | visible = List.isEmpty items |> not }, selected )

        Reset ->
            let
                ( newmodel, _ ) =
                    update items selected ShowOrHideMenu (Model model)
            in
            ( newmodel, Nothing )

        NoOp ->
            ( Model model, selected )


selectPrev : id -> List id -> Maybe id
selectPrev id ids =
    let
        getPrev cur result =
            case result of
                Err Nothing ->
                    Err (Just cur)

                Err (Just id_) ->
                    if id_ == id then
                        Ok cur

                    else
                        Err (Just cur)

                Ok id_ ->
                    Ok id_
    in
    List.foldr getPrev (Err Nothing) ids
        |> Result.toMaybe


selectNext : id -> List id -> Maybe id
selectNext id ids =
    selectPrev id (List.reverse ids)


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



-- VIEW


view : Config item -> Maybe id -> List ( id, item ) -> Model -> Html (Msg id)
view (Config config) selected items (Model model) =
    let
        viewItemWithKey ( id, item ) =
            ( toString id
            , viewItem (Config config) selected ( id, item ) (Model model)
            )

        viewMenu =
            Html.Keyed.ul
                (List.map (mapNeverToMsg NoOp) config.ul.attributes
                    ++ [ style config.ul.style ]
                    ++ [ tabindex 0
                       , onBlur HideMenu
                       ]
                )
                (List.map viewItemWithKey items)
    in
    if model.visible then
        viewMenu

    else
        text ""


viewItem : Config item -> Maybe id -> ( id, item ) -> Model -> Html (Msg id)
viewItem (Config config) selected ( id, item ) (Model model) =
    let
        listItemData =
            config.li (isSelected selected) item

        isSelected =
            Maybe.map (\id_ -> id == id_)
                >> Maybe.withDefault False
    in
    Html.li
        (List.map (mapNeverToMsg NoOp) listItemData.attributes
            ++ [ style listItemData.style ]
            ++ [ onClick (SelectItem id) ]
        )
        (List.map (Html.map (\_ -> NoOp)) listItemData.children)



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
