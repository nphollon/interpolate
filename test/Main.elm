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
  suite "Test Suite"
          [ test "First" (assertEqual 1 1)
          ]


port runner : Signal (Task a ())
port runner =
  Console.run (runDisplay allTests)
    
