import Color exposing (..)
import Debug
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.App as App
import Keyboard
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
    coords = [(5,10),(5,9),(5,8),(5,7),(5,6)],
    size = Window.Size 0 0
  }

init : (Snake, Cmd Msg)
init = (defaultSnake, Task.perform (\_ -> NoOp) Resize (Window.size))

type Msg
  = Resize Window.Size
  | Move Direction
  | NoOp

type Direction = Left | Up | Right | Down

moveSnake : Snake -> Direction -> Snake
moveSnake snake direction =
  let
    coords_ = snake.coords
    coords__ = List.take ((List.length coords_) - 1) coords_

    currentHead = List.head coords__

    coords___ = case currentHead of
      Just a ->
        let
          (x,y) = a
          newHead = case direction of
            Left -> (wrap (x - 1), y)
            Up -> (x, wrap (y + 1))
            Right -> (wrap (x + 1), y)
            Down -> (x, wrap (y - 1))
        in
          newHead :: coords__
      Nothing -> coords__
  in
    { snake | coords = coords___ }

wrap pos =
  if pos < 0 then 19
  else if pos > 19 then 0
  else pos

update : Msg -> Snake -> (Snake, Cmd Msg)
update msg snake =
  case msg of
    NoOp -> (snake, Cmd.none)
    Move direction -> (moveSnake snake direction, Cmd.none)
    Resize size -> ({ snake | size = size }, Cmd.none)

-- VIEW

backgroundColour = rgb 204 255 204
snakeColour = rgb 20 50 20

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
    draw (adjustedX,adjustedY) (rect 50 50) snakeColour

draw : (Int, Int) -> Shape -> Color -> Form
draw (x,y) shape color =
  shape
    |> filled color
    |> move (toFloat x, toFloat y)

-- SUBSCRIPTIONS

subscriptions : Snake -> Sub Msg
subscriptions snake =
  Sub.batch
    [
      Window.resizes Resize,
      Keyboard.downs keyCodeToMsg
    ]

keyCodeToMsg keyCode =
  case keyCode of
    37 -> Move Left
    38 -> Move Up
    39 -> Move Right
    40 -> Move Down
    _ -> NoOp

-- MAIN

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
