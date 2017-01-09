module UI.Menu 
  exposing 
    ( Model
    , Config
    , init
    , empty
    , Msg(..)
    , update
    , view
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


-- MODEL

type alias Model =
  { visible : Bool }

type alias Config item =
  { ul : HtmlAttributeData
  , li : Bool -> item -> HtmlAttributeDataWithChildren
  }

type alias HtmlAttributeData =
  { id : Maybe String
  , classList : List (String, Bool)
  , style : List (String, String)
  }

type alias HtmlAttributeDataWithChildren =
  { id : Maybe String
  , classList : List (String, Bool)
  , style : List (String, String)
  , children : List (Html Never)
  }

init : Bool -> Model
init visible =
  { visible = visible}

empty : Model
empty = 
  init False


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


update :  Config item -> Maybe id -> List (id, item) -> Msg id -> Model -> (Model, Maybe id)
update config selected items msg model =
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
        ( { model | visible = (List.isEmpty items |> not) }
        , newselected
        )

    SelectNextItem ->
      let 
        newselected = 
          case selected of
            Nothing -> 
              List.head items |> Maybe.map Tuple.first
            Just id -> 
              List.map Tuple.first items |> selectNext id |> Maybe.withDefault id |> Just
      in
        ( { model | visible = (List.isEmpty items |> not) }
        , newselected
        )
        
    SelectNone ->
        (model, Nothing)
        
    SelectItem id ->
      let
        newselected = 
          findById id items |> Maybe.map Tuple.first
      in
        ( { model | visible = False }
        , newselected
        )
        
    ShowMenu ->
      ( { model | visible = True }, selected )
      
    HideMenu ->
      ( { model | visible = False }, selected )
    
    ShowOrHideMenu ->
      ( { model | visible = (List.isEmpty items |> not) }, selected )

    Reset ->
      let (newmodel, _) = update config selected items ShowOrHideMenu model
      in
        ( newmodel, Nothing ) 

    NoOp -> 
      ( model, selected )

selectPrev : id -> List id -> Maybe id
selectPrev id ids =
  let 
    getPrev cur result =
      case result of
        Err Nothing -> Err (Just cur)
        Err (Just id_) -> if id_ == id then (Ok cur) else (Err (Just cur)) 
        Ok id_ -> Ok id_
  in
    List.foldr getPrev (Err Nothing) ids
      |> Result.toMaybe

selectNext : id -> List id -> Maybe id
selectNext id ids =
  selectPrev id (List.reverse ids)


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



-- VIEW

view :  Config item -> Maybe id -> List (id, item) -> Model -> Html (Msg id)
view config selected items model =
    let
        viewItemWithKey (id,item) =
            (toString id, viewItem config selected (id,item) model)
            
        viewMenu =
            Html.Keyed.ul
                [ Maybe.map id config.ul.id |> Maybe.withDefault nullAttribute
                , classList config.ul.classList
                , style config.ul.style 
                , tabindex 0
                , onBlur HideMenu
                ]
                ( List.map viewItemWithKey items )
    in
        if model.visible then viewMenu else (text "")

viewItem :  Config item -> Maybe id -> (id, item) -> Model -> Html (Msg id)
viewItem config selected (id, item) model =
    let
      customLiAttr =
          [ Maybe.map Html.Attributes.id listItemData.id
              |> Maybe.withDefault nullAttribute
          , classList listItemData.classList
          , style listItemData.style
          , onClick (SelectItem id)
          ]
      
      listItemData =
          config.li (isSelected selected) item

      isSelected = Maybe.map (\id_ -> id == id_) >> Maybe.withDefault False

    in
        Html.li customLiAttr
            (List.map (Html.map (\_ -> NoOp)) listItemData.children)


defaultMenuStyle : List ( String, String )
defaultMenuStyle =
    [ ( "position", "absolute" )
    , ( "left", "3px" )
    , ( "margin-top", "3px" )
    , ( "background", "white" )
    , ( "color", "black" )
    , ( "border", "1px solid #DDD" )
    , ( "border-radius", "3px" )
    , ( "box-shadow", "0 0 5px rgba(0,0,0,0.1)" )
    , ( "min-width", "120px" )
    , ( "z-index", "11110" )
    , ( "list-style-type", "none" )
    , ( "cursor", "pointer" )
    ]


nullAttribute : Attribute msg
nullAttribute =
  property "" JE.null
