module Interpolate.Cubic (Spline, LocalCurve, withRange
                         , valueAt, slopeAt, concavityAt, curveAt) where

import Array exposing (Array)


valueAt : Float -> Spline -> Float
valueAt =
  evaluate cubic

      
slopeAt : Float -> Spline -> Float
slopeAt =
  evaluate firstDerivative

           
concavityAt : Float -> Spline -> Float
concavityAt =
  evaluate secondDerivative


curveAt : Float -> Spline -> LocalCurve
curveAt =
  let
    curve x coeff =
      { value = cubic x coeff
      , slope = firstDerivative x coeff
      , concavity = secondDerivative x coeff
      }
  in
    evaluate curve


cubic : Float -> Coefficients -> Float
cubic x { a, b, c, d } =
  a*x^3 + b*x^2 + c*x + d


firstDerivative : Float -> Coefficients -> Float
firstDerivative x { a, b, c, d } =
  3*a*x^2 + 2*b*x + c


secondDerivative : Float -> Coefficients -> Float
secondDerivative x { a, b, c, d } =
  6*a*x + 2*b


evaluate : (Float -> Coefficients -> a) -> Float -> Spline -> a
evaluate f x (Spline spline) =
  let
    maxIndex =
      Array.length spline.coefficients - 1
           
    index =
      (x - spline.start) / spline.dx
        |> floor
        |> clamp 0 maxIndex

    offset =
      x - spline.dx * (toFloat index) - spline.start

    coeff =
      Array.get index spline.coefficients
        |> Maybe.withDefault { a = 0, b = 0, c = 0, d = 0 }
  in
    f offset coeff

      
type alias LocalCurve =
  { value : Float
  , slope : Float
  , concavity : Float
  }

                      
withRange : Float -> Float -> List Float -> Maybe Spline
withRange start end heights =
  let
    n =
      List.length heights |> toFloat
          
    dx =
      (end - start) / (n - 1)
  in
    if | n > 1 ->
         { coefficients = findCoefficients dx heights
         , start = start
         , dx = dx
         } |> Spline |> Just
    
       | otherwise ->
         Nothing


findCoefficients : Float -> List Float -> Array Coefficients
findCoefficients dx heights =
  let
    concavity y0 y1 y2 =
      6 * (y0 + y2 - 2 * y1) / dx^2

    sweep y (uPrev, vPrev) =
      (1/(4 - uPrev), (y - vPrev)/(4 - uPrev))

    backSub (u, v) mNext =
      v - u * mNext

    piece (y0, m0) (y1, m1) =
      { a = (m1 - m0) / 6 / dx
      , b = m0 / 2
      , c = (y1 - y0) / dx  -  (m1 + 2 * m0) * dx / 6
      , d = y0
      }
  in
    mapTriple concavity heights
      |> List.scanl sweep (0, 0)
      |> scanr backSub 0
      |> List.map2 (,) heights
      |> mapPair piece
      |> Array.fromList


mapTriple : (a -> a -> a -> b) -> List a -> List b
mapTriple f x0 =
  let
    tail =
      List.tail x0
          
    doubleTail =
      Maybe.andThen tail List.tail
  in
    case (tail, doubleTail) of
      (Just x1, Just x2) -> List.map3 f x0 x1 x2
      otherwise -> []


mapPair : (a -> a -> b) -> List a -> List b
mapPair f x0 =
  case (List.tail x0) of
    Just x1 -> List.map2 f x0 x1
    otherwise -> []


scanr : (a -> b -> b) -> b -> List a -> List b
scanr f init =
  List.reverse >> List.scanl f init >> List.reverse
                   

type Spline =
  Spline
    { coefficients : Array Coefficients
    , start : Float
    , dx : Float
    }
            

type alias Coefficients =
  { a : Float
  , b : Float
  , c : Float
  , d : Float
  }
            

{- To implement:

withDelta : Float -> Float -> List Float -> Spline

slopeAt : Float -> Spline -> Float

concavityAt : Float -> Spline -> Float

curveAt : Float -> Spline -> { value : Float, slope : Float, concavity : Float }

type Boundary = Natural | Parabolic | Cubic | Periodic | Clamped Float Float
-}
