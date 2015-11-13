{- -
  This program will not work in the browser.
  To run on the command line, execute run-tests.sh
- -}

module Main where

import Text
import Task exposing (Task)

import ElmTest.Test exposing (Test, suite, test)
import ElmTest.Assertion exposing (assert, assertEqual, assertNotEqual)
import ElmTest.Runner.Console exposing (runDisplay)

import Console

import Interpolate.Cubic as Cubic
import Interpolate.Bicubic as Bicubic
import Matrix exposing (Matrix)

allTests : Test
allTests =
  suite "Cubic spline interpolation"
        [ suite "One-dimensional"
                  [ testDegenerate
                  , testLinear
                  , testCubic
                  ]
        , suite "Two-dimensional"
                  [ testMatrix
                  , testDataFactory
                  , testSurface
                  , testContinuity
                  ]
        ]


testDegenerate : Test
testDegenerate =
  let
    emptySpline =
      Cubic.withRange 2 10 []

    flatSpline =
      Cubic.withRange 1 10 [ 5 ]
  in
    suite "not enough data"
            [ testCurve "empty data set yields y = 0" 1 emptySpline
                          { value = 0, slope = 0, concavity = 0 }
                          
            , testCurve "one point yields y = x0 above x0" 2 flatSpline
                          { value = 5, slope = 0, concavity = 0 }

            , testCurve "one point yields y = x0 at x0" 1 flatSpline
                          { value = 5, slope = 0, concavity = 0 }

            , testCurve "one point yields y = x0 below x0" 0 flatSpline
                          { value = 5, slope = 0, concavity = 0 }
            ]

-- y = 1/2 x^3 - 3/2 x^2 + 1
-- for x < 1, f(x) = y(1 - x)
-- for 1 < x, f(x) = y(x - 1)
testCubic : Test
testCubic =
  let
    spline = Cubic.withRange 0 2 [0, 1, 0]
  in
    suite "three data points"
            [ testCurve "x = 0.0" 0 spline
                        { value = 0, slope = 1.5, concavity = 0 }
                        
            , testCurve "x = 0.5" 0.5 spline
                        { value = 0.6875, slope = 1.125, concavity = -1.5 }
                        
            , testCurve "x = 1.0" 1 spline
                        { value = 1, slope = 0, concavity = -3 }

            , testCurve "x = 2.0" 2 spline
                        { value = 0, slope = -1.5, concavity = 0 }

            , testCurve "x = 3.0" 3 spline
                        { value = -1, slope = 0, concavity = 3 }
            ]


-- tests that y = x + 1
testLinear : Test
testLinear =
  let
    testPoints name line =
      suite name
              [ testCurve "at x0" 0 line
                            { value = 1, slope = 1, concavity = 0 }
                            
              , testCurve "at x1" 1 line
                            { value = 2, slope = 1, concavity = 0 }
                            
              , testCurve "at midpoint" 0.5 line
                            { value = 1.5, slope = 1, concavity = 0 }
              ]
  in
    suite "two data points"
            [ testPoints "data at 0 and 1" (Cubic.withRange 0 1 [1, 2])
            , testPoints "data at 0 and 2" (Cubic.withRange 0 2 [1, 3])
            , testPoints "data at -1 and 1" (Cubic.withRange -1 1 [0, 2])
            ]
                         

testCurve : String -> Float -> Cubic.Spline -> Cubic.LocalCurve -> Test
testCurve name x spline expected =
  suite name
          [ test "value" (assertEqual expected.value (Cubic.valueAt x spline))
          , test "slope" (assertEqual expected.slope (Cubic.slopeAt x spline))
          , test "concavity" (assertEqual expected.concavity (Cubic.concavityAt x spline))
          , test "local curve" (assertEqual expected (Cubic.curveAt x spline))
          ]
      

testDataFactory : Test
testDataFactory =
  let
    assertBad =
      Bicubic.rows >> assertEqual Nothing

    assertGood =
      Bicubic.rows >> assertNotEqual Nothing
  in
    suite "building two-dimensional data sets"
            [ test "empty list returns Nothing"
                     (assertBad [])
                     
            , test "list of empty lists returns Nothing"
                     (assertBad [[]])
                     
            , test "sublists of different lengths returns Nothing"
                     (assertBad [[1], [2, 3]])
                     
            , test "singleton list returns something"
                     (assertGood [[1]])
                     
            , test "sublists of same length returns something"
                     (assertGood [[1], [2]])
            ]


testSurface : Test
testSurface =
  let
    data =
      [ [5, 3]
      , [2, 6]
      ]

    spline =
      Bicubic.rows data
        |> Maybe.withDefault Bicubic.emptyData
        |> Bicubic.withRange {x=1,y=2} {x=3,y=6}

    assertValueAt point expected =
      assertEqual expected (Bicubic.valueAt point spline)

    testSurface name point expected =
      suite name
              [ test "value" (assertEqual expected.value (Bicubic.valueAt point spline))
              , test "gradient" (assertEqual expected.gradient (Bicubic.gradientAt point spline))
              , test "laplacian" (assertEqual expected.laplacian (Bicubic.laplacianAt point spline))
              , test "local surface" (assertEqual expected (Bicubic.surfaceAt point spline))
              ]
  in
    suite "6x'y' - 2x' - 3y' + 5, where x' = (x-1)/2 and y' = (y-2)/4"
            [ testSurface "at start point"
                            { x = 1, y = 2 }
                            { value = 5, gradient = { x = -1, y = -0.75 }, laplacian = 0 }

            , testSurface "at end point"
                            { x = 3, y = 6 }
                            { value = 6, gradient = { x = 2, y = 0.75 }, laplacian = 0 }
                            
            , testSurface "beyond data region"
                            { x = 4, y = 0 }
                            { value = -1, gradient = { x = -2.5, y = 1.5 }, laplacian = 0 }
            ]


testContinuity : Test
testContinuity =
  let
    data =
      [ [ 5, 3, 5 ]
      , [ 2, 6, 3 ]
      , [ 1, 3, 7 ]
      ]

    spline =
      Bicubic.rows data
        |> Maybe.withDefault Bicubic.emptyData
        |> Bicubic.withDelta {x = -1, y = -1 } { x = 1, y = 1 }

    aboutEqual a b =
      (a - b)^2 < 0.01

    surfaceAt pt =
      Bicubic.surfaceAt pt spline
             
    testSmooth name a b =
      suite name
              [ test "values" <| assert
                       <| aboutEqual a.value b.value
              , test "x derivatives" <| assert
                       <| aboutEqual a.gradient.x b.gradient.x
              , test "y derivatives" <| assert
                       <| aboutEqual a.gradient.y b.gradient.y
              , test "laplacians" <| assert
                       <| aboutEqual a.laplacian b.laplacian
              ]
  in
    suite "surface should be smooth at knots"
          [ testSmooth "smooth across x boundary"
                 (surfaceAt { x = -0.001, y = -0.5 })
                 (surfaceAt { x = 0.001, y = -0.5 })
                 
          , testSmooth "smooth across y boundary"
                 (surfaceAt { x = 0.5, y = -0.001 })
                 (surfaceAt { x = 0.5, y = 0.001 })
                 
          , testSmooth "smooth across knot, northwest to southeast"
                 (surfaceAt { x = -0.001, y = -0.001 })
                 (surfaceAt { x = 0.001, y = 0.001 })
                 
          , testSmooth "smooth across knot, northeast to southwest"
                 (surfaceAt { x = 0.001, y = -0.001 })
                 (surfaceAt { x = -0.001, y = 0.001 })
          ]

                 
testMatrix : Test
testMatrix =
  let
    varied =
      Matrix.fromRows [[1, 2], [3, 4]] |> withDefault

    uniform =
      Matrix.repeat 3 4 8

    withDefault =
      Maybe.withDefault uniform
  in
    suite "Matrix utilities"
            [ test "rows slices matrix horizontally"
                     <| assertEqual
                          [[1, 2], [3, 4]]
                          (Matrix.rows varied)
                          
            , test "fromColumns is transpose of fromRows"
                     <| assertEqual
                          (Just [[1,3],[2,4]])
                          (Matrix.fromColumns [[1,2],[3,4]]
                             |> Maybe.map Matrix.rows)
                          
            , test "columns slices matrix vertically"
                     <| assertEqual
                          [[1, 3], [2, 4]]
                          (Matrix.columns varied)
                          
            , test "repeat builds a uniform m x n matrix"
                     <| assertEqual
                          [[8,8,8],[8,8,8],[8,8,8],[8,8,8]]
                          (uniform |> Matrix.rows)
                          
            , test "width is width"
                     <| assertEqual 3
                          (uniform |> Matrix.width)
                          
            , test "height is height"
                     <| assertEqual 4
                          (uniform |> Matrix.height)
                          
            , test "transpose reverse rows and columns"
                     <| assertEqual
                          [[1, 3], [2, 4]]
                          (Matrix.transpose varied |> Matrix.rows)
                          
            , test "get accesses an element"
                     <| assertEqual
                          (Just 3)
                          (Matrix.get 0 1 varied)
                          
            , test "set mutates an element"
                     <| assertEqual
                          [[1, 7], [3, 4]]
                          (Matrix.set 1 0 7 varied |> Matrix.rows)
                          
            , test "map mutates all elements"
                     <| assertEqual
                          [[2, 4], [6, 8]]
                          (Matrix.map ((*) 2) varied |> Matrix.rows)

            , test "indexedMap provides indexes to mapping function"
                     <| assertEqual
                          [ [ (0, 0, 1), (1, 0, 2) ]
                          , [ (0, 1, 3), (1, 1, 4) ]
                          ]
                          (Matrix.indexedMap (,,) varied |> Matrix.rows)
              
            , test "map4 combines 4 matrixes"
                     <| assertEqual
                          (Just [[(1,5,9,13),(2,7,11,14)],[(3,6,10,15),(4,8,12,16)]])
                          (Matrix.map4 (,,,)
                                 (Matrix.fromRows [[1, 2], [3, 4]] |> withDefault)
                                 (Matrix.fromColumns [[5, 6], [7, 8]] |> withDefault)
                                 (Matrix.fromColumns [[9, 10], [11, 12]] |> withDefault)
                                 (Matrix.fromRows [[13, 14], [15, 16]] |> withDefault)
                           |> Maybe.map Matrix.rows
                          )

            , test "map4 fails if matrixes are different sizes"
                     <| assertEqual
                          Nothing
                          (Matrix.map4 (,,,)
                                 (Matrix.fromRows [[1]] |> withDefault)
                                 (Matrix.fromRows [[5]] |> withDefault)
                                 (Matrix.fromRows [[4]] |> withDefault)
                                 (Matrix.fromRows [[2, 3]] |> withDefault)
                          )
                     
            , test "times multiplies matrixes"
                     <| assertEqual
                          (Just [[8, 5], [20, 13], [32, 21]])
                          (Matrix.times
                                   (Matrix.fromRows [[1, 2], [3, 4], [5, 6]] |> withDefault)
                                   (Matrix.fromRows [[4, 3], [2, 1]] |> withDefault)
                           |> Maybe.map Matrix.rows
                          )

            , test "height of first operand must equal width of second"
                     <| assertEqual
                          Nothing
                          (Matrix.times
                                   (Matrix.fromRows [[4, 3], [2, 1]] |> withDefault)
                                   (Matrix.fromRows [[1, 2], [3, 4], [5, 6]] |> withDefault)
                          )

            , test "quadCollapse on a 2x2 matrix returns 1x1 matrix"
                     <| assertEqual
                          ([ [ { n = { w = 3, e = 2 }
                               , s = { w = 0 , e = 1 }
                               }
                             ]
                           ]
                          )
                          (Matrix.fromRows [[3, 2], [0, 1]]
                             |> withDefault
                             |> Matrix.quadCollapse
                             |> Matrix.rows
                          )
                     
            , test "quadCollapse on a 3x4 matrix returns a 2x3 matrix"
                     <| assertEqual
                          ([ [ { n = {w=1,e=2}, s = {w=4,e=5} }
                             , { n = {w=2,e=3}, s = {w=5,e=6} }
                             ]
                           , [ { n = {w=4,e=5}, s = {w=7,e=8} }
                             , { n = {w=5,e=6}, s = {w=8,e=9} }
                             ]
                           , [ { n = {w=7,e=8}, s = {w=10,e=11} }
                             , { n = {w=8,e=9}, s = {w=11,e=12} }
                             ]
                           ]
                          )
                          (Matrix.fromRows [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]
                             |> withDefault
                             |> Matrix.quadCollapse
                             |> Matrix.rows
                          )
                     
            , test "sum adds all elements of a float matrix"
                   <| assertEqual
                        10
                        (Matrix.sum varied)
            ]
             
port runner : Signal (Task a ())
port runner =
  Console.run (runDisplay allTests)
    
