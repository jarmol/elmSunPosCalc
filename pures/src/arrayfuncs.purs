module Main where

import Prelude
import Data.Array (filter, range)
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (logShow, log)

import TryPureScript (render, withConsole)

a :: Int -> String
a n = "Nr " <> show n

b :: Int -> Int 
b n = n * n 

ar10 :: Array Int 
ar10 = range 1 10

inverts :: Array Number
inverts = [2.0, 4.0, 5.0, 10.0] <#> \n -> 1.0 / n 

pipern :: Array Number
pipern = [2.0, 3.0, 4.0, 5.0, 6.0, 10.0] <#> \n -> 180.0 / n :: Number

evenonly :: Array Int 
evenonly = filter (\n -> n `mod` 2 == 0) ar10

evens :: Array Int
evens = map (\n -> if mod n 2 == 0 then n else 0) ar10


main :: Effect Unit
main = render =<< withConsole do
  logShow ( map a [1,2,3] )
  log $ "Squares " <> show (map b ar10)
  log $ "Inverted " <> show inverts
  log $ "π/n in grades " <> show pipern
  log $ "Even true or false " <> show evens
  log $ "Even only " <> show evenonly
  log $ "Sum of array cells " <> show (foldl (+) 0 ar10)
  log $ "Multiply array cells " <> show (foldl (*) 1 [1,2,3,4,5])
  
