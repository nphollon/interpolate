module Interpolate.Cubic (Spline, LocalCurve, withRange
                         , valueAt, slopeAt, concavityAt, curveAt) where

{-| This library interpolates cubic splines for one-dimensional sets of data.

It computes "natural splines", which means the second derivative at the endpoints
is set to zero.

For more information on the mathematics used, check out
[this paper.](http://web.archive.org/web/20090408054627/http://online.redwoods.cc.ca.us/instruct/darnold/laproj/Fall98/SkyMeg/Proj.PDF)

# Creating splines
@docs withRange, Spline

# Interpolating
@docs valueAt, slopeAt, concavityAt, curveAt, LocalCurve
-}

import Array exposing (Array)


{-| Given `x1` and a spline `f(x)`, return `f(x1)`
-}
valueAt : Float -> Spline -> Float
valueAt =
  evaluate cubic

           
{-| Return the first derivative of the curve at the given point
-}
slopeAt : Float -> Spline -> Float
slopeAt =
  evaluate firstDerivative


{-| Return the second derivative of the curve at the given point
-}
concavityAt : Float -> Spline -> Float
concavityAt =
  evaluate secondDerivative


{-| Return a record containing the value, slope, and concavity of the curve
at the given point
-}
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


{-|-}
type alias LocalCurve =
  { value : Float
  , slope : Float
  , concavity : Float
  }


{-| Compute a spline, given the minimum and maximum values of `x` and a
list of data for `f(x)`. The data should be evenly spaced and in order of
increasing `x`. 

For example, if we have the data

    f(2) = 1
    f(3) = 5.2
    f(4) = 3.2
    f(5) = 0.8

Then we would generate a spline by calling

    fSpline = withRange 2 6 [ 1, 5.2, 3.2, 0.8 ]

If there is only one data point, then the spline will be a horizontal line
passing through that point. If the data is empty, the spline will be zero
everywhere.
-}
withRange : Float -> Float -> List Float -> Spline
withRange start end heights =
  let
    n =
      List.length heights |> toFloat
          
    dx =
      (end - start) / (n - 1)
  in
    if | 1 < n ->
         { coefficients = findCoefficients dx heights
         , start = start
         , dx = dx
         } |> Spline

       | otherwise ->
         { coefficients = degenerateCoefficients heights
         , start = 0
         , dx = 0
         } |> Spline


degenerateCoefficients : List Float -> Array Coefficients
degenerateCoefficients heights =
  { a = 0
  , b = 0
  , c = 0
  , d = List.head heights |> Maybe.withDefault 0
  } |> Array.repeat 1
  

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
                   

{-|-}
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
