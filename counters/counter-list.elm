import Counter
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List



main =
  App.beginnerProgram
    { model = init
    , update = update
    , view = view
    }



-- MODEL


type alias Model =
  { counters : List IndexedCounter
  , uid : Int
  }


type alias IndexedCounter =
  { id : Int
  , model : Counter.Model
  }


init : Model
init =
  { counters = []
  , uid = 0
  }



-- UPDATE


type Msg
  = Insert
  | Remove
  | Modify Int Counter.Msg


update : Msg -> Model -> Model
update message ({counters, uid} as model) =
  case message of
    Insert ->
      { model
        | counters = counters ++ [ IndexedCounter uid (Counter.init 0) ]
        , uid = uid + 1
      }

    Remove ->
      { model | counters = List.drop 1 counters }

    Modify id msg ->
      { model | counters = List.map (updateHelp id msg) counters }


updateHelp : Int -> Counter.Msg -> IndexedCounter -> IndexedCounter
updateHelp targetId msg {id, model} =
  IndexedCounter id (if targetId == id then Counter.update msg model else model)



-- VIEW


view : Model -> Html Msg
view model =
  let
    remove =
      button [ onClick Remove, buttonStyle ] [ text "Remove" ]

    insert =
      button [ onClick Insert, buttonStyle ] [ text "Add" ]

    counters =
      List.map viewIndexedCounter model.counters
  in
     div [ containerStyle ]
      [ h1 [ headerStyle ] [ text "counter mayhem" ]
      , div []
        ([remove, insert] ++ counters)
      ]


viewIndexedCounter : IndexedCounter -> Html Msg
viewIndexedCounter {id, model} =
  App.map (Modify id) (Counter.view model)


containerStyle : Attribute msg
containerStyle =
  style
    [ ("width", "400px")
    , ("margin", "150px auto")
    , ("padding", "40px")
    , ("border", "1px solid #000")
    , ("text-align", "center")
    ]


headerStyle : Attribute msg
headerStyle =
  style
    [ ("font-family", "Courier New")
    , ("margin-bottom", "50px")
    ]


buttonStyle : Attribute msg
buttonStyle =
  style
    [ ("height", "3em")
    , ("width", "5em")
    , ("margin", "0 15px")
    , ("font-size", "1.1em")
    , ("font-family", "Courier New")
    , ("text-transform", "lowercase")
    , ("background-color", "#000")
    , ("color", "#FFF")
    , ("border", "0")
    , ("border-radius", "0.5em")
    ]
