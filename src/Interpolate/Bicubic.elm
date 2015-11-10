module Interpolate.Bicubic (Data, Point, Spline
                           , rows, emptyData, withRange
                           , valueAt, gradientAt, surfaceAt) where

import Array exposing (Array)

import Matrix exposing (Matrix)

import Interpolate.Cubic as Cubic


rows : List (List Float) -> Maybe Data
rows points =
  let
    headLength =
      List.head points
        |> Maybe.map List.length
        |> Maybe.withDefault 0
  in
    if | headLength == 0 -> Nothing
       | otherwise -> Matrix.fromList points |> Maybe.map Data


emptyData : Data
emptyData =
  Matrix.repeat 1 1 0 |> Data
            

valueAt : Point -> Spline -> Float
valueAt point (Spline spline) =
  0


gradientAt : Point -> Spline -> Point
gradientAt a b = a


surfaceAt : Point -> Spline -> LocalSurface
surfaceAt a b =
  { value = valueAt a b
  , gradient = gradientAt a b
  }


type alias LocalSurface =
  { value : Float
  , gradient : Point
  }

          
withRange : Point -> Point -> Data -> Spline
withRange start end (Data data) =
  let
    deltaFor dim elem =
      ((elem end) - (elem start)) / (toFloat n - 1)

    delta =
      { x = deltaFor Matrix.width .x
      , y = deltaFor Matrix.height .y
      }

    bigEnough =
      (Matrix.width data > 1) && (Matrix.height data > 1)
  in
    if | bigEnough ->
         { coeff = findCoefficients delta data
         , start = start
         , delta = delta
         } |> Spline

       | otherwise ->                   
         { coeff = degenerateCoefficients delta data
         , start = { x = 0, y = 0 }
         , delta = { x = 0, y = 0 }
         } |> Spline


findCoefficients : Point -> Matrix Float -> Matrix Coefficients
findCoefficients delta data =
  findDerivatives delta data
  |> quadCollapse
  |> Matrix.map fromDerivatives


findDerivatives : Point -> Matrix Float -> Matrix Derivatives
findDerivatives delta data = _


quadCollapse : Matrix a -> Matrix (Quad a)
quadCollapse data = _


fromDerivatives : Quad Derivatives -> Coefficients
                   
  
type Data =
  Data (Matrix Float)

                
type alias Point =
  { x : Float
  , y : Float
  }


type Spline =
  Spline { coeff : Matrix Coefficients
         , start : Point
         , delta : Point
         }

  
type alias Coefficients =
  Matrix Float


type alias Derivatives =
  { z = Float
  , dz_dx = Float
  , dz_dy = Float
  , dz_dx_dy = Float
  }


type alias Quad a =
  { n : { w : a, e : a }
  , s : { w : a, e : a }
  }
