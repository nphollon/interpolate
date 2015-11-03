{- -
  This program will not work in the browser.
  To run on the command line, execute run-tests.sh
- -}

module Main where

import Text
import Task exposing (Task)

import ElmTest.Test exposing (Test, suite, test)
import ElmTest.Assertion exposing (assertEqual)
import ElmTest.Runner.Console exposing (runDisplay)

import Console

import Interpolate.Cubic as Cubic

allTests : Test
allTests =
  suite "Cubic spline interpolation"
          [ testDegenerate
          , testLinear
          , testCubic
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
      

port runner : Signal (Task a ())
port runner =
  Console.run (runDisplay allTests)
    
