module Main where

import Color exposing(..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Mouse
import Random
import Signal exposing (Signal, Address)
import String
import Window

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update model0 input)

input : Signal Action
input = Signal.sampleOn Mouse.isDown (Signal.map m2act Mouse.position)

m2act : (Int,Int) -> Action
m2act (x,y) =   Add {x=x,y=y}

-- MODEL

type alias Coord = {x:Int, y:Int}
type alias Model = {coords:List Coord, seed: Random.Seed}
model0 = {coords=[], seed=Random.initialSeed 0}

-- UPDATE

type Action = Reset | Add Coord

update : Action -> Model -> Model
update action model =
  case action of
    Reset -> model0
    Add x -> {model | coords = x::model.coords}

-- VIEW

view : (Int,Int) -> Model -> Element
view (w,h) {coords} =
  let h' = h-100
  in flow down [
  collage w (h') (List.map (draw (w,h')) coords),
  headShow coords, headRel (w,h') coords]

headShow xs = case xs of
  [] -> show ""
  (x::_) -> show x
headRel wh xs = case xs of
  [] -> show ""
  ({x,y}::_) -> show (relative wh (x,y))


draw : (Int,Int) -> Coord -> Form
draw wh {x,y} =
  let
  (rx,ry) = relative wh (x,y)
  in circle 20 |> filled clearGrey |> move (toFloat rx, toFloat ry)

relative (w,h) (x,y) = (x - w//2, h//2 - y)

clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6
