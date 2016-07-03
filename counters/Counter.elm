module Counter exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = Int

init : Int -> Model
init count =
  count


-- UPDATE

type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

view : Model -> Html Msg
view model =
  div [ containerStyle ]
    [ button [ onClick Decrement, buttonStyle ] [ text "–" ]
    , div [ countStyle  ] [ text (toString model) ]
    , button [ onClick Increment, buttonStyle ] [ text "+" ]
    ]

containerStyle : Attribute msg
containerStyle =
  style
    [ ("margin", "20px 0")
    , ("line-height", "50px")
    ]

countStyle : Attribute msg
countStyle =
  style
    [ ("font-size", "40px")
    , ("font-family", "Courier New")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    , ("vertical-align", "middle")
    , ("margin", "0 10px")
    ]

buttonStyle : Attribute msg
buttonStyle =
  style
    [ ("height", "50px")
    , ("width", "4em")
    , ("background-color", "#000")
    , ("color", "#FFF")
    , ("border", "0")
    , ("border-radius", "0.5em")
    , ("font-size", "1.1em")
    ]
