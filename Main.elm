import Color exposing (..)
import Debug
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.App as App
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
    coords = [(3,3), (3,4)],
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

view : Snake -> Html Msg
view ({coords, size} as snake) =
  toHtml <|
  container size.width size.height middle <|
  collage 1000 1000
    [
      rect 1000 10000 |> filled backgroundColour
    ]

--make obj shape color =
  --shape
    --|> filled color
    --|> move (obj.x,obj.y)

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
