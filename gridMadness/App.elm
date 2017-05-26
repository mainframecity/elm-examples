module App exposing (..)

import Html exposing (Html, Attribute, div, h1, text, program)
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode)
import Array exposing (Array)


-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


gridDimensions : Position
gridDimensions =
    ( 6, 6 )


type alias Model =
    { playerPosition : Position
    , grid : Grid
    , keyPressed : Maybe KeyCode
    , showMeta : Bool
    }


type alias Grid =
    Array GridRow


type alias GridRow =
    Array GridBox


type GridBox
    = Player
    | Empty


type alias Position =
    ( Int, Int )


type Direction
    = Up
    | Down
    | Left
    | Right
    | UpLeft
    | UpRight
    | DownLeft
    | DownRight
    | Stay


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initPlayerPosition : Position
initPlayerPosition =
    ( 1, 1 )


initModel : Model
initModel =
    Model initPlayerPosition initGrid Nothing False


initGrid : Grid
initGrid =
    let
        ( _, y ) =
            gridDimensions
    in
        (Array.repeat y initGridRow)
            |> updateGrid ( 0, 0 ) ( 1, 1 )


initGridRow : GridRow
initGridRow =
    let
        ( x, _ ) =
            gridDimensions
    in
        Array.repeat x Empty



-- MESSAGES


type Msg
    = NoOp
    | KeyMsg KeyCode



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyMsg 191 ->
            ( { model | showMeta = not model.showMeta }, Cmd.none )

        KeyMsg code ->
            let
                direction =
                    toDirection code

                newPosition =
                    movePlayer model.playerPosition direction
            in
                ( Model
                    newPosition
                    (updateGrid model.playerPosition newPosition model.grid)
                    (Just code)
                    model.showMeta
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyMsg



-- VIEW


view : Model -> Html Msg
view model =
    div [ styleContainer ]
        [ div [ styleHeader ] [ h1 [ styleTitle ] [ text "Grid Madness!" ] ]
        , viewGrid model
        , div [ (styleMetaInfo True) ]
            [ text "h/j/k/l to move" ]
        , div [ (styleMetaInfo model.showMeta) ]
            [ viewKeyPress model.keyPressed
            , viewPlayerPos model.playerPosition
            ]
        , div [ (styleMetaInfo (not model.showMeta)) ]
            [ text "press ? for debug info"
            ]
        ]


viewGrid : Model -> Html Msg
viewGrid model =
    div [ styleGrid ]
        (Array.toList
            (Array.map viewGridRow model.grid)
        )


viewGridRow : GridRow -> Html Msg
viewGridRow row =
    div [ styleGridRow ]
        (Array.toList
            (Array.map viewGridBox row)
        )


viewGridBox : GridBox -> Html Msg
viewGridBox box =
    div [ (styleGridBox box) ] []


viewKeyPress : Maybe KeyCode -> Html a
viewKeyPress code =
    div [ styleInnerMeta ]
        [ textKeyPress code ]


textKeyPress : Maybe KeyCode -> Html a
textKeyPress code =
    case code of
        Just keyCode ->
            text ("key-code pressed: " ++ (toString keyCode))

        Nothing ->
            text "waiting..."


viewPlayerPos : Position -> Html msg
viewPlayerPos position =
    div [ styleInnerMeta ]
        [ text ("pos: " ++ (toString position)) ]



-- STYLE


styleContainer : Attribute msg
styleContainer =
    style
        [ ( "height", "100%" )
        ]


styleHeader : Attribute msg
styleHeader =
    style
        [ ( "width", "240px" )
        , ( "margin", "10px auto 0 auto" )
        , ( "text-align", "center" )
        ]


styleTitle : Attribute msg
styleTitle =
    style
        [ ( "text-transform", "lowercase" )
        , ( "font-family", "Courier New, monospace" )
        , ( "font-size", "1.9em" )
        ]


styleMetaInfo : Bool -> Attribute msg
styleMetaInfo showMeta =
    let
        visibility =
            if showMeta then
                "block"
            else
                "none"
    in
        style
            [ ( "width", "240px" )
            , ( "margin", "20px auto" )
            , ( "margin-top", "80px" )
            , ( "text-align", "center" )
            , ( "font-family", "Courier New, monospace" )
            , ( "display", visibility )
            ]


styleInnerMeta : Attribute msg
styleInnerMeta =
    style
        [ ( "margin-top", "15px" )
        ]


styleGrid : Attribute msg
styleGrid =
    style
        [ ( "margin", "80px auto" )
        , ( "width", "240px" )
        , ( "height", "240px" )
        , ( "box-sizing", "border-box" )
        , ( "background-color", "#000" )
        ]


styleGridRow : Attribute msg
styleGridRow =
    style
        [ ( "clear", "left" )
        , ( "box-sizing", "border-box" )
        , ( "background-color", "#000" )
        ]


styleGridBox : GridBox -> Attribute msg
styleGridBox box =
    let
        bgColor =
            case box of
                Player ->
                    "#fff"

                Empty ->
                    "#000"
    in
        style
            [ ( "height", "40px" )
            , ( "width", "40px" )
            , ( "box-sizing", "border-box" )
            , ( "float", "left" )
            , ( "background-color", bgColor )
            ]



-- MOVEMENT


toDirection : KeyCode -> Direction
toDirection code =
    case code of
        72 ->
            Left

        74 ->
            Down

        75 ->
            Up

        76 ->
            Right

        89 ->
            UpLeft

        85 ->
            UpRight

        66 ->
            DownLeft

        78 ->
            DownRight

        _ ->
            Stay


movePlayer : Position -> Direction -> Position
movePlayer ( x, y ) direction =
    case direction of
        Left ->
            ( x - 1, y ) |> clampMovement

        Right ->
            ( x + 1, y ) |> clampMovement

        Up ->
            ( x, y - 1 ) |> clampMovement

        Down ->
            ( x, y + 1 ) |> clampMovement

        UpLeft ->
            ( x - 1, y - 1 ) |> clampMovement

        UpRight ->
            ( x + 1, y - 1 ) |> clampMovement

        DownLeft ->
            ( x - 1, y + 1 ) |> clampMovement

        DownRight ->
            ( x + 1, y + 1 ) |> clampMovement

        Stay ->
            ( x, y )


clampMovement : Position -> Position
clampMovement ( x, y ) =
    let
        ( xMax, yMax ) =
            gridDimensions
    in
        ( (clamp 0 (xMax - 1) x), (clamp 0 (yMax - 1) y) )


updateGrid : Position -> Position -> Grid -> Grid
updateGrid oldPosition newPosition grid =
    grid
        |> setOnGrid oldPosition Empty
        |> setOnGrid newPosition Player


setOnGrid : Position -> GridBox -> Grid -> Grid
setOnGrid position box grid =
    let
        ( x, y ) =
            position
    in
        grid
            |> Array.get y
            |> Maybe.withDefault (Array.fromList [])
            |> Array.set x box
            |> (\row -> Array.set y row grid)
