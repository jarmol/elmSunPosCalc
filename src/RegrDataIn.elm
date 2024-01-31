module RegrDataIn exposing (main)

import Html exposing (Html,div,h1,h2,p,hr,text,a,li)
import Html.Attributes as HA
import DataXY exposing (xy)

readData : (List Float, List Float)
readData = List.unzip xy

-- Weight kg
weight : List Float
weight = Tuple.first readData

-- Height cm
height : List Float
height = Tuple.second readData

sumx = List.sum weight -- sum of x variables
nx = List.length weight |> toFloat -- number of x values
mx = sumx / nx  -- average of x variables (kg weight)


sumy = List.sum height
ny = List.length height |> toFloat
my = sumy / ny -- Average height cm

avgWeight = fixTo3val mx
avgHeight = fixTo3val my

stdWeight = fixTo3val 
  <| myStdDev weight 

stdHeight = fixTo3val 
  <| myStdDev height 

myStdDev : List Float -> Float
myStdDev lsx =
      let nr = toFloat <| List.length lsx
          sumlsx = List.sum lsx
          meanx = sumlsx / nr 
          listvarlsx = List.map (\x -> ( x - meanx )^2 ) lsx
      in  sqrt <| (List.sum listvarlsx) / nr 


vsqrW = myStdDev weight

fixTo2val : Float -> Float
fixTo2val x =
        (toFloat <| round (100 * x)) / 100

fixTo3val : Float -> Float
fixTo3val x =
        (toFloat <| round (1000 * x)) / 1000

xy_ = List.map2 (\x y -> x*y) weight height
sumxy = List.sum xy_
x2 = List.map (\x -> x*x) weight
sumx2 = List.sum x2
y2 = List.map (\y -> y*y) height
sumy2 = List.sum y2

-- b0 = (ΣYΣX² – ΣX(ΣXY)) / (nΣX² – (ΣX)²)
b0Equation = "Equation β0 = ( ΣY·ΣX² – ΣX·ΣXY ) / ( n·ΣX² – (ΣX)² )"
b0 = (sumy*sumx2 - sumx*sumxy) / (nx*sumx2 - sumx^2)

b1Equation = "Equation β1 = ( n·ΣXY – ΣX·ΣY ) / ( n·ΣX² – (ΣX)² )"
b1 = (nx*sumxy - sumx*sumy) / (nx*sumx2 - sumx^2)

-- Test f(x average) == y average
xavg = String.fromFloat <| fixTo3val mx
yavg = String.fromFloat <| fixTo3val <| (b0 + b1 * mx)

-- The estimated linear regression equation is: ŷ = b0 + b1*x
-- ye = b0 + b1*x

yestim : List Float
yestim = List.map (\xi -> b0 + b1*xi) weight

estim2 : List Float 
estim2 = List.map fixTo2val yestim

delta : List Float 
delta = List.map2 (\y e -> y - e) height yestim

vsqrE : Float
vsqrE = fixTo3val <| myStdDev delta

r2 : Float
r2 = 1.0 - ((myStdDev delta) / (myStdDev height))^2

correl : String
correl = String.fromFloat <| fixTo3val <| sqrt r2

listAsString : List Float -> String
listAsString myList =
    List.map String.fromFloat myList
        |> List.map (\el -> " " ++ el)
        |> List.foldl (++) " "

main = Html.div [HA.style "margin" "5%"
        , HA.style "font-family" "Helvetica Neue", HA.style "color" "blue"] [
        p [HA.style "font-size" "32px"][text "Linear regression Weight [kg] / Height [cm]"
        ]
        , p [HA.style "font-size" "24px"] [text "Weight data [kg] *)"]
        , p [HA.style "font-size" "18px"][text (listAsString weight
          ++ " Average = " ++ String.fromFloat avgWeight
          ++ " Standard deviation = " 
          ++ String.fromFloat stdWeight )] 
        , p [HA.style "font-size" "24px"] [text "Height data [cm] *)"]
        , p [HA.style "font-size" "18px"][text (listAsString height
          ++ " Average = " ++ String.fromFloat avgHeight
          ++ " Standard deviation = " ++ String.fromFloat stdHeight )]
        , p [HA.style "font-size" "24px"] [text "The estimated linear regression equation: ŷ = β0 + β1·x"]
        , div [HA.style "font-size" "1.5em", HA.style "color" "green"] [
         text ("ŷ = " 
         ++ String.fromFloat (fixTo3val b0) 
         ++ " + "
         ++ String.fromFloat (fixTo3val b1) ++ "·x")
         ]
        , p [][text ("Testing the sample midpoint (x average, y average) == (" ++ xavg ++ ", " ++ yavg ++ ") is on the regression line")]
        , p [][text ("The estimated values ŷ : " ++  listAsString  estim2)]
        , p [][text ("Std. dev. of the linear model is "
          ++ String.fromFloat vsqrE)]
        , p [][text ("Coefficient of Determination R² = "
         ++ String.fromFloat (fixTo3val r2)
         ++ ", correlation r = " ++ correl)]
        , hr [][]
        , p [][text ("Calculation of " ++ b0Equation)]
        , p [][text ("Calculation of " ++ b1Equation)]
        , p [][text ("*) Note, all data is imported from the external file src/DataXY.elm")]
        , li [] [a [HA.href "https://www.statology.org/linear-regression-by-hand/"][text "How to perform linear regression by hand"]]
        , li [] [a [HA.href "https://www.scribbr.com/statistics/pearson-correlation-coefficient/"][text "Pearson correlation coefficient"]]
        ]
-- https://ellie-app.com/q8MxR35ZTWta1

-- List.map2 Tuple.pair weight height
-- [(140,60),(155,62),(159,67),(179,70),(192,71),(200,72),(212,75)]
