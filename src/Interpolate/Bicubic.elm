module Interpolate.Bicubic (Data, Vector, Spline, LocalSurface
                           , rows, emptyData, withRange, withDelta
                           , valueAt, gradientAt, surfaceAt, laplacianAt) where

{-| This module uses [bicubic splines](https://en.wikipedia.org/wiki/Bicubic_interpolation)
to interpolate surfaces. Bicubic interpolation is the 2D equivalent
of cubic interpolation.

For example, if we have the following data:

    y=|
    9 | 1.9  2.1  5.5
    6 | 2.2  3.1  5.4
    3 | 3.8  4.0  4.3
    0 | 1.1  2.3  3.8
    -----------------
     x=  2    4    6

We could construct a 2D spline this way:

    data = rows [ [ 1.1, 2.3, 3.8 ]
                , [ 3.8, 4.0, 4.3 ]
                , [ 2.2, 3.1, 5.4 ]
                , [ 1.9, 2.1, 5.5 ]
                ]
             |> Maybe.withDefault emptyData

    start = { x = 2, y = 0 }
    end = { x = 6, y = 9 }
    delta = { x = 2, y = 3 }

    -- These two splines are equivalent
    splineOne = withRange start end data
    splineTwo = withDelta start delta data


# Creating data sets
@docs rows, emptyData, Data

# Creating splines
@docs withRange, withDelta, Vector, Spline

# Interpolating
@docs valueAt, gradientAt, laplacianAt, surfaceAt, LocalSurface
-}

import Array exposing (Array)

import Interpolate.Cubic as Cubic
import Matrix exposing (Matrix, Quad)


{-| Construct a two-dimensional data set. The input is given in x-major order,
so a grid of values like this:

    f(0,0) = 1
    f(1,0) = 2
    f(0,1) = 3
    f(1,1) = 4

Should be passed to the function in this format:

    rows [ [1,2], [3,4] ] -- returns Just Data

If every row has the same length, the function returns a `Data` object that
can be used to build a spline. If the rows are uneven (or empty), it returns
`Nothing`.

    rows [ [1,1,1], [1,1,1] ] -- returns Just Data

    rows [ [1], [1], [1] ] -- returns Just Data

    rows [ [1,1,1], [1] ] -- returns Nothing

    rows [ [], [] ] -- returns Nothing

    rows [ ] -- returns Nothing
-}

rows : List (List Float) -> Maybe Data
rows =
  Matrix.fromRows >> Maybe.map Data


{-| An empty data set. Useful as a default value when working 
with the `rows` function. If you create a spline from it, the
spline will be zero everywhere.
-}
emptyData : Data
emptyData =
  Data defaultMatrix
       
           
{-| Evaluate the spline at the given point -}
valueAt : Vector -> Spline -> Float
valueAt =
  evaluate cubic


{-| Compute the [gradient](https://en.wikipedia.org/wiki/Gradient)
of the spline at the given point. The gradient is the x and y
partial derivatives of the spline.
-}
gradientAt : Vector -> Spline -> Vector
gradientAt =
  evaluate del


{-| Compute the [Laplacian](https://en.wikipedia.org/wiki/Laplace_operator)
of the spline at the given point. The Laplacian is the divergence
of the gradient. It is computed by adding the x and y second partial
derivatives. -}
laplacianAt : Vector -> Spline -> Float
laplacianAt =
  evaluate delDotDel


{-| Returns `valueAt`, `gradientAt`, and `laplacianAt` results in
a single record.
-}
surfaceAt : Vector -> Spline -> LocalSurface
surfaceAt =
  evaluate (\pt coeff ->
              { value = cubic pt coeff
              , gradient = del pt coeff
              , laplacian = delDotDel pt coeff
              }
           )


evaluate : (Vector -> Coefficients -> a) -> Vector -> Spline -> a
evaluate f point (Spline spline) =
  let
    maxIndex =
      { x = Matrix.width spline.coefficients - 1
      , y = Matrix.height spline.coefficients - 1
      }
    
    recentered =
      pointMap (-) point spline.start

    normalized =
      pointMap (/) recentered spline.delta

    index =
      pointMap (floor >> flip (clamp 0)) normalized maxIndex

    offset =
      pointMap (toFloat >> (*)) index spline.delta
        |> pointMap (-) recentered
  in
    Matrix.get index.x index.y spline.coefficients
      |> Maybe.withDefault defaultMatrix
      |> f offset

         
cubic : Vector -> Coefficients -> Float
cubic { x, y } =
  addMonomials (\i j factor ->
                  factor * x^i * y^j
               )

                 
del : Vector -> Coefficients -> Vector
del { x, y } coeff =
  let
    dz_dx i j factor =
      if 0 < i then
        factor * i * x^(i-1) * y^j
      else
        0

    dz_dy i j factor =
      if 0 < j then
        factor * j * x^i * y^(j-1)
      else
        0
  in
    { x = addMonomials dz_dx coeff
    , y = addMonomials dz_dy coeff
    }


delDotDel : Vector -> Coefficients -> Float
delDotDel { x, y } coeff =
  let
    d2z_dx2 i j factor =
      if 1 < i then
        factor * i * (i - 1) * x^(i-2) * y^j
      else
        0

    d2z_dy2 i j factor =
      if 1 < j then
        factor * j * (j - 1) * x^i * y^(j-2)
      else
        0

    derivTotal i j factor =
      (d2z_dx2 i j factor) + (d2z_dy2 i j factor)
  in
    addMonomials derivTotal coeff


addMonomials : (Float -> Float -> Float -> Float) -> Coefficients -> Float
addMonomials monomial coeff =
  Matrix.indexedMap (\i j -> monomial (toFloat i) (toFloat j)) coeff
    |> Matrix.sum
                       

{-| Construct a spline, given the positions of the lower left (min-x, min-y)
and upper right (max-x, max-y) data samples, and a data set.
-}
withRange : Vector -> Vector -> Data -> Spline
withRange start end (Data data) =
  let
    deltaFor dim elem =
      ((elem end) - (elem start)) / (toFloat (dim data) - 1)

    delta =
      { x = deltaFor Matrix.width .x
      , y = deltaFor Matrix.height .y
      }

    bigEnough =
      (Matrix.width data > 1) && (Matrix.height data > 1)
  in
    if bigEnough then
      withDelta start delta (Data data)
    else
      { coefficients = degenerateCoefficients data
      , start = { x = 0, y = 0 }
      , delta = { x = 0, y = 0 }
      } |> Spline


{-| Construct a spline, given the position of the lower left data sample
and the dimensions of a grid cell.
-}
withDelta : Vector -> Vector -> Data -> Spline
withDelta start delta (Data data) =
  { coefficients = findCoefficients delta data
  , start = start
  , delta = delta
  } |> Spline


degenerateCoefficients : Matrix Float -> Matrix Coefficients
degenerateCoefficients data =
  let
    height =
      Matrix.get 0 0 data
        |> Maybe.withDefault 0
  in
    Matrix.repeat 4 4 0
      |> Matrix.set 0 0 height
      |> Matrix.repeat 1 1

         
findCoefficients : Vector -> Matrix Float -> Matrix Coefficients
findCoefficients delta data =
  findDerivatives data
  |> Maybe.map Matrix.quadCollapse
  |> Maybe.map (Matrix.map (fromDerivatives delta))
  |> Maybe.withDefault (degenerateCoefficients data)


findDerivatives : Matrix Float -> Maybe (Matrix Derivatives)
findDerivatives data =
  let
    xDerivs =
      Matrix.rows data
        |> List.map (Cubic.withDelta 0 1)
        |> List.map (slopes (Matrix.width data))
        |> Matrix.fromRows
        |> withDefaultMatrix

    yDerivs =
      Matrix.columns data
        |> List.map (Cubic.withDelta 0 1)
        |> List.map (slopes (Matrix.height data))
        |> Matrix.fromColumns
        |> withDefaultMatrix

    xyDerivs =
      Matrix.columns xDerivs
        |> List.map (Cubic.withDelta 0 1)
        |> List.map (slopes (Matrix.height data))
        |> Matrix.fromColumns
        |> withDefaultMatrix
    
    pack z dz_dx dz_dy dz_dx_dy =
      { z = z
      , dz_dx = dz_dx
      , dz_dy = dz_dy
      , dz_dx_dy = dz_dx_dy
      }
  in
    Matrix.map4 pack data xDerivs yDerivs xyDerivs


slopes : Int -> Cubic.Spline -> List Float
slopes n spline =
  (toFloat >> flip Cubic.slopeAt spline)
    |> Array.initialize n 
    |> Array.toList

         
fromDerivatives : Vector -> Quad Derivatives -> Coefficients
fromDerivatives delta d =
  let
    derivsMatrix =
      [ [ d.n.w.z
        , d.n.e.z
        , d.s.w.z
        , d.s.e.z
        , d.n.w.dz_dx
        , d.n.e.dz_dx
        , d.s.w.dz_dx
        , d.s.e.dz_dx
        , d.n.w.dz_dy
        , d.n.e.dz_dy
        , d.s.w.dz_dy
        , d.s.e.dz_dy
        , d.n.w.dz_dx_dy
        , d.n.e.dz_dx_dy
        , d.s.w.dz_dx_dy
        , d.s.e.dz_dx_dy
        ]
      ] |> Matrix.fromColumns |> withDefaultMatrix
    
    factors =
      [ [ 1, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0 ]
      , [ 0, 0, 0, 0,   1, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0 ]
      , [-3, 3, 0, 0,  -2,-1, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0 ]
      , [ 2,-2, 0, 0,   1, 1, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0 ]
        
      , [ 0, 0, 0, 0,   0, 0, 0, 0,   1, 0, 0, 0,   0, 0, 0, 0 ]
      , [ 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,   1, 0, 0, 0 ]
      , [ 0, 0, 0, 0,   0, 0, 0, 0,  -3, 3, 0, 0,  -2,-1, 0, 0 ]
      , [ 0, 0, 0, 0,   0, 0, 0, 0,   2,-2, 0, 0,   1, 1, 0, 0 ]

      , [-3, 0, 3, 0,   0, 0, 0, 0,  -2, 0,-1, 0,   0, 0, 0, 0 ]
      , [ 0, 0, 0, 0,  -3, 0, 3, 0,   0, 0, 0, 0,  -2, 0,-1, 0 ]
      , [ 9,-9,-9, 9,   6, 3,-6,-3,   6,-6, 3,-3,   4, 2, 2, 1 ]
      , [-6, 6, 6,-6,  -3,-3, 3, 3,  -4, 4,-2, 2,  -2,-2,-1,-1 ]

      , [ 2, 0,-2, 0,   0, 0, 0, 0,   1, 0, 1, 0,   0, 0, 0, 0 ]
      , [ 0, 0, 0, 0,   2, 0,-2, 0,   0, 0, 0, 0,   1, 0, 1, 0 ]
      , [-6, 6, 6,-6,  -4,-2, 4, 2,  -3, 3,-3, 3,  -2,-1,-2,-1 ]
      , [ 4,-4,-4, 4,   2, 2,-2,-2,   2,-2, 2,-2,   1, 1, 1, 1 ]
      ] |> Matrix.fromRows |> withDefaultMatrix

    scale i j factor =
      factor / delta.x^(toFloat i) / delta.y^(toFloat j)
  in
    Matrix.times factors derivsMatrix
      |> Maybe.map squarify
      |> Maybe.map (Matrix.indexedMap scale)
      |> withDefaultMatrix


squarify : Matrix Float -> Matrix Float
squarify matrix =
  let
    slices flat =
      [ Array.slice 0 4 flat
      , Array.slice 4 8 flat
      , Array.slice 8 12 flat
      , Array.slice 12 16 flat
      ] |> List.map Array.toList
  in
    Matrix.columns matrix
      |> List.concat
      |> Array.fromList
      |> slices
      |> Matrix.fromRows
      |> withDefaultMatrix


withDefaultMatrix : Maybe (Matrix Float) -> Matrix Float
withDefaultMatrix =
  Maybe.withDefault defaultMatrix


defaultMatrix : Matrix Float
defaultMatrix =
  Matrix.repeat 1 1 0


pointMap : (a -> b -> c) -> { x:a, y:a } -> { x:b, y:b } -> { x:c, y:c }
pointMap op a b =
  { x = op a.x b.x
  , y = op a.y b.y
  }


{-|-}
type alias LocalSurface =
  { value : Float
  , gradient : Vector
  , laplacian : Float
  }


{-|-}
type Data =
  Data (Matrix Float)


{-| Stores data with an x and a y component.
-}
type alias Vector =
  { x : Float
  , y : Float
  }

                  
{-|-}
type Spline =
  Spline { coefficients : Matrix Coefficients
         , start : Vector
         , delta : Vector
         }

  
type alias Coefficients =
  Matrix Float


type alias Derivatives =
  { z : Float
  , dz_dx : Float
  , dz_dy : Float
  , dz_dx_dy : Float
  }
