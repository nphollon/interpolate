module Matrix (Matrix, Quad
              , fromRows, fromColumns, repeat
              , width, height
              , rows, columns
              , get, set
              , map, map4, transpose, times, quadCollapse) where

import Array exposing (Array)


fromRows : List (List a) -> Maybe (Matrix a)
fromRows rows =
  let
    headWidth =
      List.head rows
        |> Maybe.map List.length
        |> Maybe.withDefault 0

    isSameWidth row =
      List.length row == headWidth
  in
    if | headWidth == 0 -> Nothing

       | List.all isSameWidth rows ->
         { width = headWidth
         , height = List.length rows
         , data = addIndex rows
         } |> Just
        
       | otherwise -> Nothing


addIndex : List (List a) -> List ((Int, Int), a)
addIndex rows =
  let
    indexItem j i item =
      ((i, j), item)
      
    indexRow j row =
      List.indexedMap (indexItem j) row
  in
    List.indexedMap indexRow rows
      |> List.concat
           
           
fromColumns : List (List a) -> Maybe (Matrix a)
fromColumns =
  fromRows >> Maybe.map transpose


repeat : Int -> Int -> a -> Matrix a
repeat width height value =
  let
    makeItem i j =
      ((i, j), value)

    makeRow i =
      Array.initialize height (makeItem i)
        |> Array.toList

    data =
      Array.initialize width makeRow
        |> Array.toList
        |> List.concat
  in
    { width = width
    , height = height
    , data = data
    }


width : Matrix a -> Int
width = .width


height : Matrix a -> Int
height = .height


get : Int -> Int -> Matrix a -> Maybe a
get x y matrix =
  let
    findItem ((i, j), item) oldResult =
      if | oldResult /= Nothing -> oldResult
         | (i == x) && (j == y) -> Just item
         | otherwise -> Nothing
  in
    List.foldl findItem Nothing matrix.data


set : Int -> Int -> a -> Matrix a -> Matrix a
set x y newItem matrix =
  let
    insert indexedItem =
      if | (x,y) == fst indexedItem -> ((x,y), newItem)
         | otherwise -> indexedItem
  in
    { matrix | data <- List.map insert matrix.data }
           
quadCollapse : Matrix a -> Matrix (Quad a)
quadCollapse _ = def  


map : (a -> b) -> Matrix a -> Matrix b
map f matrix =
  let
    new (position, value) =
      (position, f value)
  in
    { matrix | data <- List.map new matrix.data }


map4 : (a -> b -> c -> d -> e) ->
       Matrix a -> Matrix b -> Matrix c -> Matrix d -> Matrix e
map4 _ _ _ _ _ = def


rows : Matrix a -> List (List a)
rows matrix =
  let
    init =
      Array.repeat matrix.height []

    
    iter ((i, j), item) rows =
      case (Array.get j rows) of
        Just row -> Array.set j (item :: row) rows
        Nothing -> rows
  in
    List.foldr iter init matrix.data
      |> Array.toList


columns : Matrix a -> List (List a)
columns = transpose >> rows


times : Matrix Float -> Matrix Float -> Matrix Float
times _ _ = def


transpose : Matrix a -> Matrix a
transpose original =
  let
    swapIndexes ((i, j), x) =
      ((j, i), x)
  in
    { width = original.height
    , height = original.width
    , data = List.map swapIndexes original.data
    }
              

type alias Matrix a =
  { width : Int
  , height : Int
  , data : List ((Int, Int), a)
  }

def = { width = 0, height = 0, data = [] }

type alias Quad a =
  { n : { w : a, e : a }
  , s : { w : a, e : a }
  }
