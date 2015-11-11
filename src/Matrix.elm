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
       Matrix a -> Matrix b -> Matrix c -> Matrix d -> Maybe (Matrix e)
map4 f m1 m2 m3 m4 =
  let
    new (p, v1) (_, v2) (_, v3) (_, v4) =
      (p, f v1 v2 v3 v4)

    data =
      List.map4 new m1.data m2.data m3.data m4.data
      
    dim matrix =
      (matrix.width, matrix.height)
      
    sameSize =
      dim m1 == dim m2 &&
      dim m1 == dim m3 &&
      dim m1 == dim m4
  in
    if | sameSize -> Just { m1 | data <- data }
       | otherwise -> Nothing
              

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


times : Matrix Float -> Matrix Float -> Maybe (Matrix Float)
times a b =
  let
    bColumns =
      columns b

    aRows =
      rows a

    dotProduct row column =
      List.sum (List.map2 (*) row column)

    dotColumns columns row =
      List.map (dotProduct row) columns
  in
    if | a.width == b.height ->
         fromRows (List.map (dotColumns bColumns) aRows)
         
       | otherwise -> Nothing


transpose : Matrix a -> Matrix a
transpose original =
  let
    swapIndexes ((i, j), x) =
      ((j, i), x)

    compareIndexes ((i1, j1), _) ((i2, j2), _) =
      case compare j1 j2 of
        LT -> LT
        EQ -> compare i1 i2
        GT -> GT        
      
    data =
      List.map swapIndexes original.data
        |> List.sortWith compareIndexes
  in
    { width = original.height
    , height = original.width
    , data = data
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
