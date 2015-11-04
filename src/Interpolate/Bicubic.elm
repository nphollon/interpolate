module Interpolate.Bicubic (Data, Point, Spline
                           , rows, emptyData, withRange, valueAt) where

import Array exposing (Array)

import Interpolate.Cubic as Cubic


rows : List (List Float) -> Maybe Data
rows points =
  let
    lengths =
      List.map List.length points
          
    headLength =
      List.head lengths
        |> Maybe.withDefault 0
  in
    if | headLength == 0 -> Nothing
       | List.any ((/=) headLength) lengths -> Nothing
       | otherwise -> Just (Data points)


emptyData : Data
emptyData =
  Data [ [ 0 ] ]
            

valueAt : Point -> Spline -> Float
valueAt point (Spline spline) =
  let
    yData = List.map (Cubic.valueAt point.x) spline.xSplines
  in
    Cubic.withRange spline.yStart spline.yEnd yData
      |> Cubic.valueAt point.y

          
withRange : Point -> Point -> Data -> Spline
withRange start end (Data data) =
  { xSplines =  List.map (Cubic.withRange start.x end.x) data
  , yStart = start.y
  , yEnd = end.y
  } |> Spline    


type Data =
  Data (List (List Float))

                
type alias Point =
  { x : Float
  , y : Float
  }


type Spline =
  Spline { xSplines : List Cubic.Spline
         , yStart : Float
         , yEnd : Float
         }
