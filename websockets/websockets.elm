import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket



main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


echoServer : String
echoServer =
  "wss://echo.websocket.org"



-- MODEL


type alias Model =
  { input : String
  , messages : List String
  }


init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)



-- UPDATE


type Msg
  = Input String
  | Send
  | NewMessage String


update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, messages} =
  case msg of
    Input newInput ->
      (Model newInput messages, Cmd.none)

    Send ->
      (Model "" messages, WebSocket.send echoServer input)

    NewMessage str ->
      (Model input (str :: messages), Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen echoServer NewMessage



-- VIEW


view : Model -> Html Msg
view model =
  div [ containerStyle ]
    [ h1 [ headerStyle ] [ text "websocket beatdown" ]
    , input [ onInput Input, textBoxStyle, value model.input ] []
    , button [ onClick Send, buttonStyle ] [ text "Send" ]
    , div [] (List.map viewMessage model.messages)
    ]


viewMessage : String -> Html msg
viewMessage msg =
  div [ messageStyle ] [ text msg ]


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


textBoxStyle : Attribute msg
textBoxStyle =
  style
    [ ("height", "3em")
    , ("padding", "0 20px")
    , ("vertical-align", "middle")
    , ("font-size", "1.1em")
    , ("font-family", "Courier New")
    ]


messageStyle : Attribute msg
messageStyle =
  style
    [ ("margin", "15px 0 0 0")
    , ("padding", "10px 20px")
    , ("border", "1px solid #EBEBEB")
    , ("font-family", "Courier New")
    ]
