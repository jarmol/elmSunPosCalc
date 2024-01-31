module DataXY exposing (xy)

import List.Extra exposing (zip)

weight : List Float
weight = [3.3, 3.36, 3.46, 3.03, 2.87, 3.59, 3.42, 3.82, 3.02, 3.63, 4.15]

height : List Float
height = [50.4, 54.4, 45.2, 47.2, 43.7, 54.9, 54.2, 48.4, 49.7, 53.1, 55.1]

xy : List (Float, Float)
xy = zip (List.reverse weight) (List.reverse height)

