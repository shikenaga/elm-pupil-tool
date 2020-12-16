module Main exposing (main)

import Browser
import Browser.Events
import Browser.Dom
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Json.Decode exposing (..)
import Task

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { mouseCursor:
      { x: Int
      , y: Int
      }
    
  , screen:
      { w: Int
      , h: Int
      }
  }

init : () -> (Model, Cmd Msg)
init =
  always (
    { mouseCursor =
      { x = 0
      , y = 0
      }   
    , screen =
      { w = 800
      , h = 800
      }
    }
    , Task.perform
      (\{ viewport } -> ScreenSize (round viewport.width) (round viewport.height))
      Browser.Dom.getViewport
  )
-- UPDATE

type Msg
  = Move Int Int
  | ScreenSize Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move x y ->
      ( { model | mouseCursor =
          { x = x
          , y = y
          }
        }
        , Cmd.none
      )
    ScreenSize w h ->
      ( { model | screen =
          { w = w
          , h = h
          }
        }
        , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize (\w h -> ScreenSize w h)

-- VIEW

type alias CircleInfo =
  { x : Int
  , y : Int
  , radius : Int
  }

type alias PupilInfo =
  { w : Int
  , h : Int
  }

view : Model -> Html Msg
view model =
  div
    []
    [ svg
      [ width (String.fromInt model.screen.w)
      , height (String.fromInt model.screen.h)
      , viewBox ("0 0 " ++ String.fromInt model.screen.w  ++ String.fromInt model.screen.h)
      , onMouseMove Move
      ]
      ( eye
          (CircleInfo
            (round (toFloat model.screen.w * 0.45))
            (round (toFloat model.screen.h * 0.2))
            (round (toFloat model.screen.w * 0.05))
          )
          (PupilInfo
            (round (toFloat model.screen.w * 0.008))
            (round (toFloat model.screen.w * 0.015))
          )
          (model.mouseCursor.x, model.mouseCursor.y)
        ++
        eye
          (CircleInfo
            (round (toFloat model.screen.w * 0.55))
            (round (toFloat model.screen.h * 0.2))
            (round (toFloat model.screen.w * 0.05))
          )
          (PupilInfo
            (round (toFloat model.screen.w * 0.008))
            (round (toFloat model.screen.w * 0.015))
          )
          (model.mouseCursor.x, model.mouseCursor.y)
      )
    ]

onMouseMove : (Int -> Int -> msg) -> Svg.Attribute msg
onMouseMove msg =
  Svg.Events.on "mousemove"
    (Json.Decode.map2 msg
      (Json.Decode.field "clientX" Json.Decode.int)
      (Json.Decode.field "clientY" Json.Decode.int)
    )

distance : (Int, Int) -> (Int, Int) -> (Int, Float)
distance (x1, y1) (x2, y2) =
  let
    distanceX = toFloat (x2 - x1)
    distanceY = toFloat (y2 - y1)
  in
    ( round (sqrt (distanceX ^ 2 + distanceY ^ 2))
    , atan2 distanceY distanceX
    )

conductPupil : CircleInfo -> Float -> (Int, Int) -> (Int, Int)
conductPupil rangeInfo velocity (x , y) =
  let
    (centerToCursor_, radiansOToPointer) =
      distance (rangeInfo.x, rangeInfo.y) (x, y)

    centerToCursor = round(toFloat centerToCursor_ * velocity)

    calculateCoordinate : Int -> Float -> (Int, Int)
    calculateCoordinate dist rad =
      ( round (toFloat dist * (cos rad))
      , round (toFloat dist * (sin rad))
      )
  in
    if rangeInfo.radius > centerToCursor then
      calculateCoordinate centerToCursor radiansOToPointer
    else
      calculateCoordinate rangeInfo.radius radiansOToPointer
    
eye : CircleInfo -> PupilInfo -> (Int, Int) -> List (Svg Msg)
eye circleInfo pupilInfo (mouseX, mouseY) =
  let
    (pupilX, pupilY) = conductPupil
      (CircleInfo 
        circleInfo.x
        circleInfo.y
        (round ((toFloat circleInfo.radius) * 0.7))
      )
      0.1
      (mouseX, mouseY)
  in
    [
      circle
        [ cx (String.fromInt circleInfo.x)
        , cy (String.fromInt circleInfo.y)
        , r (String.fromInt circleInfo.radius)
        , stroke "black"
        , fill "#fff"
        , strokeWidth "2" 
        ]
        []
      , ellipse
        [ cx (String.fromInt (pupilX + circleInfo.x))
        , cy (String.fromInt (pupilY + circleInfo.y))
        , rx (String.fromInt pupilInfo.w)
        , ry (String.fromInt pupilInfo.h)
        ]
        []
    ]