module Main where

import Graphics.Element as Element


main : Signal Element.Element
main = 
  Signal.foldp update init input |> Signal.map view


    
type alias Model =
  {}

  
init : Model
init =
  {}



type Update =
  Dummy {}

           
input : Signal Update
input =
  Signal.map Dummy (Signal.constant {})



update : Update -> Model -> Model
update up model =
  case up of
    Dummy a ->
      model



view : Model -> Element.Element
view model = 
  Element.show model








                  
