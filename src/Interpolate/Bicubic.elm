module Interpolate.Bicubic (Data, Point, Spline
                           , rows, emptyData, withRange
                           , valueAt, gradientAt, surfaceAt) where

import Array exposing (Array)

import Interpolate.Cubic as Cubic
import Matrix exposing (Matrix, Quad)


rows : List (List Float) -> Maybe Data
rows =
  Matrix.fromRows >> Maybe.map Data


emptyData : Data
emptyData =
  Data defaultMatrix
       
           
valueAt : Point -> Spline -> Float
valueAt =
  evaluate cubic


gradientAt : Point -> Spline -> Point
gradientAt =
  evaluate del


surfaceAt : Point -> Spline -> LocalSurface
surfaceAt a b =
  { value = valueAt a b
  , gradient = gradientAt a b
  }


type alias LocalSurface =
  { value : Float
  , gradient : Point
  }

                        
evaluate : (Point -> Coefficients -> a) -> Point -> Spline -> a
evaluate f point (Spline spline) =
  let
    xIndex =
      (point.x - spline.start.x) / spline.delta.x
        |> floor
        |> clamp 0 (Matrix.width spline.coefficients - 1)

    yIndex =
      (point.y - spline.start.y) / spline.delta.y
        |> floor
        |> clamp 0 (Matrix.height spline.coefficients - 1)

    xOffset =
      point.x - (toFloat xIndex * spline.delta.x) - spline.start.x

    yOffset =
      point.y - (toFloat yIndex * spline.delta.y) - spline.start.y

    coeff =
      Matrix.get xIndex yIndex spline.coefficients
        |> Maybe.withDefault defaultMatrix
  in
    f { x = xOffset, y = yOffset } coeff


cubic : Point -> Coefficients -> Float
cubic { x, y } =
  addMonomials (\i j factor ->
                  factor * x^i * y^j
               )

                 
del : Point -> Coefficients -> Point
del { x, y } coeff =
  let
    dz_dx i j factor =
      if | (0 < i) -> factor * i * x^(i-1) * y^j
         | otherwise -> 0

    dz_dy i j factor =
      if | (0 < j) -> factor * j * x^i * y^(j-1)
         | otherwise -> 0
  in
    { x = addMonomials dz_dx coeff
    , y = addMonomials dz_dy coeff
    }


addMonomials : (Float -> Float -> Float -> Float) -> Coefficients -> Float
addMonomials monomial coeff =
  Matrix.indexedMap (\i j -> monomial (toFloat i) (toFloat j)) coeff
    |> Matrix.sum
                       
                     
withRange : Point -> Point -> Data -> Spline
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
    if | bigEnough ->
         { coefficients = findCoefficients delta data
         , start = start
         , delta = delta
         } |> Spline

       | otherwise ->                   
         { coefficients = degenerateCoefficients data
         , start = { x = 0, y = 0 }
         , delta = { x = 0, y = 0 }
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

         
findCoefficients : Point -> Matrix Float -> Matrix Coefficients
findCoefficients delta data =
  findDerivatives delta data
  |> Maybe.map Matrix.quadCollapse
  |> Maybe.map (Matrix.map fromDerivatives)
  |> Maybe.withDefault (degenerateCoefficients data)


findDerivatives : Point -> Matrix Float -> Maybe (Matrix Derivatives)
findDerivatives delta data =
  let
    xDerivs =
      Matrix.rows data
        |> List.map (Cubic.withDelta 0 delta.x)
        |> List.map (slopes delta.x (Matrix.width data))
        |> Matrix.fromRows
        |> withDefaultMatrix

    yDerivs =
      Matrix.columns data
        |> List.map (Cubic.withDelta 0 delta.y)
        |> List.map (slopes delta.y (Matrix.height data))
        |> Matrix.fromColumns
        |> withDefaultMatrix

    xyDerivs =
      Matrix.columns xDerivs
        |> List.map (Cubic.withDelta 0 delta.y)
        |> List.map (slopes delta.y (Matrix.height data))
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


slopes : Float -> Int -> Cubic.Spline -> List Float
slopes dx n spline =
  let
    slope i = Cubic.slopeAt (toFloat i * dx) spline
  in
    Array.initialize n slope |> Array.toList

         
fromDerivatives : Quad Derivatives -> Coefficients
fromDerivatives d =
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
  in
    Matrix.times factors derivsMatrix
      |> Maybe.map squarify
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
        
  
type Data =
  Data (Matrix Float)

                
type alias Point =
  { x : Float
  , y : Float
  }


type Spline =
  Spline { coefficients : Matrix Coefficients
         , start : Point
         , delta : Point
         }

  
type alias Coefficients =
  Matrix Float


type alias Derivatives =
  { z : Float
  , dz_dx : Float
  , dz_dy : Float
  , dz_dx_dy : Float
  }
