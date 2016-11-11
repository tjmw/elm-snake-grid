import Color exposing (..)
import Debug
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.App as App
import List
import Task
import Window

type alias Snake =
  {
    coords : List (Int, Int),
    size: Window.Size
  }

defaultSnake : Snake
defaultSnake =
  {
    coords = [(1,2),(1,3),(2,3),(3,3),(3,4)],
    size = Window.Size 0 0
  }

init : (Snake, Cmd Msg)
init = (defaultSnake, Task.perform (\_ -> NoOp) Resize (Window.size))

type Msg
  = Resize Window.Size
  | NoOp

update : Msg -> Snake -> (Snake, Cmd Msg)
update msg snake =
  case msg of
    NoOp -> (snake, Cmd.none)
    Resize size -> ({ snake | size = size }, Cmd.none)

-- VIEW

textColour = rgb 150 150 150
backgroundColour = rgb 204 255 204
gridWidth = 1000
gridHeight = 1000

view : Snake -> Html Msg
view ({coords, size} as snake) =
  toHtml <|
  container size.width size.height middle <|
  collage gridWidth gridHeight <|
    List.concat [
      [rect gridWidth gridHeight |> filled backgroundColour],
      List.map fillGrid coords
    ]

fillGrid : (Int, Int) -> Form
fillGrid (x,y) =
  let
    adjustedX = -(500) + (x * 50) + 25
    adjustedY = -(500) + (y * 50) + 25
  in
    draw (adjustedX,adjustedY) (rect 50 50) black

draw : (Int, Int) -> Shape -> Color -> Form
draw (x,y) shape color =
  shape
    |> filled color
    |> move (toFloat x, toFloat y)

-- SUBSCRIPTIONS

subscriptions : Snake -> Sub Msg
subscriptions snake =
  Sub.batch
    [ Window.resizes Resize ]

-- MAIN

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
