import Counter
import Html exposing (Html, Attribute, button, div, text, h1)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)



main =
  App.beginnerProgram
    { model = init 0 0
    , update = update
    , view = view
    }



-- MODEL


type alias Model =
  { topCounter : Counter.Model
  , bottomCounter : Counter.Model
  }


init : Int -> Int -> Model
init top bottom =
  { topCounter = Counter.init top
  , bottomCounter = Counter.init bottom
  }



-- UPDATE


type Msg
  = Reset
  | Swap
  | Top Counter.Msg
  | Bottom Counter.Msg


update : Msg -> Model -> Model
update message model =
  case message of
    Reset ->
      init 0 0

    Swap ->
      init model.bottomCounter model.topCounter

    Top msg ->
      { model | topCounter = Counter.update msg model.topCounter }

    Bottom msg ->
      { model | bottomCounter = Counter.update msg model.bottomCounter }



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ containerStyle ]
    [ h1 [ headerStyle ] [ text "counter magic" ]
    , App.map Top (Counter.view model.topCounter)
    , App.map Bottom (Counter.view model.bottomCounter)
    , div [ buttonBoxStyle ]
      [ button [ onClick Reset, buttonStyle ] [ text "Reset" ]
      , button [ onClick Swap, buttonStyle ] [ text "Swap" ]
      ]
    ]


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

buttonBoxStyle : Attribute msg
buttonBoxStyle =
  style
    [ ("margin-top", "25px")
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
