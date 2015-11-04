module Interpolate.Bicubic (data) where

data : List (List Float) -> Maybe ()
data points =
  let
    lengths =
      List.map List.length points
          
    headLength =
      List.head lengths
        |> Maybe.withDefault 0
  in
    if | headLength == 0 -> Nothing
       | List.any ((/=) headLength) lengths -> Nothing
       | otherwise -> Just ()
