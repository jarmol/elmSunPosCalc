module Zip exposing (zip)

zip: List a -> List b -> List (a,b)
zip a b =
  List.map2 (\x y -> (x,y)) a b

-- Make a list of tuples from two lists 
