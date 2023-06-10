module Main where

import Prelude
import Data.Array (filter, range)
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (logShow, log)
import Data.Number (sin, cos, pi)
import Data.Number.Format (toStringWith, fixed)

import TryPureScript (render, withConsole)

a :: Int -> String
a n = "Nr " <> show n

b :: Int -> Int 
b n = n * n 

ar10 :: Array Int 
ar10 = range 1 10

decar :: Array Number
decar = [2.0, 3.0, 4.0, 5.0, 6.0, 10.0]

pidivs :: Array String
pidivs = map (\x -> "π/" <> show x) decar 

sinvals :: Array Number
sinvals = map (\n -> sin (pi / n) ) decar

cosvals :: Array Number
cosvals = map (\n -> cos (pi / n) ) decar

fixsinvals :: Array String
fixsinvals = map (\n -> toStringWith (fixed 3) n) sinvals

fixcosvals :: Array String
fixcosvals = map (\n -> toStringWith (fixed 3) n) cosvals

inverts :: Array Number
inverts = [2.0, 4.0, 5.0, 10.0] <#> \n -> 1.0 / n 

pipern :: Array Number
pipern = decar <#> \n -> 180.0 / n :: Number

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
  log $ "π/n table " <> show pidivs
  log $ "sin(π/n) table " <> show fixsinvals
  log $ "cos(π/n) table " <> show fixcosvals
  log $ "Even true or false " <> show evens
  log $ "Even only " <> show evenonly
  log $ "Sum of array cells " <> show (foldl (+) 0 ar10)
  log $ "Multiply array cells " <> show (foldl (*) 1 [1,2,3,4,5])

