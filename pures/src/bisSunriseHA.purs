module Solar.Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Number (sin, cos, asin, acos, tan, pi)
import Data.Decimal (Decimal, fromInt, fromNumber, toNumber, toFixed, modulo)
import TryPureScript (render, withConsole)

-- Defining infix operator same as in Elm '//'for division of integers

infix 8 negdiv as //

-- Function for Julian day number JDN calculated from the given date
-- where y = year, m = month, d = day

jdnGr :: Int -> Int -> Int -> Int 
jdnGr y m d = 
     (1461 * (y + 4800 + (m - 14) // 12)) // 4
        + (367 * (m - 2 - 12 * (m - 14) // 12)) // 12
        - (3 * ((y + 4900 + (m - 14) // 12) // 100) // 4)
        + d - 32075

-- Defining new function 'negdiv' replacing 'div' to give same results
-- as Elm integer-division '//'also for negative dividend
negdiv :: Int -> Int -> Int
negdiv n  m = if n < 0 then -(div (-n) m)
              else div n m



main :: Effect Unit
main  =
  render =<< withConsole do
  log $  ("TESTING JULIAN DATE NUMBERS 'JDN' AND INFIX OPERATOR '//'")
  log $  (printAB 2023 2 24) -- 2460000
  log $  "Not leap year"
  log $  (printAB 2020 2 28)
  log $  (printAB 2023 3 1)
  log $ "Leap year 2020 end February, begin March"
  log $  (printAB 2020 2 29)
  log $  (printAB 2020 3 1)
  log $  (printAB 2000 1 1) -- 2451545
  log $  "Year length, no leap year"
  log $  show ((jdnGr 2024 1 1) - (jdnGr 2023 1 1)) <> " days"
  log $  "Length of leap year"
  log $  show ((jdnGr 2025 1 1) - (jdnGr 2024 1 1)) <> " days"
  log $  "Julian day " <> (stringA 2023 5 5)
         <> " 1:27:58 JD = " 
         <>  toFixed 6 (jdateGr 2023 5 5 1 27 58)
  log $  "Century " <> toFixed 9 cent -- 0.233389763
  log $  "Mean anomaly of Sun "
     <> toFixed 6 meanAnomal <> "°"
  log $  "Mean anom normalized "
     <> toFixed 6 normAnomal <> "°"
  log $  "Mean longitude of Sun "
     <> toFixed 6 meanLongitude <> "°"
  log $  "Mean longitude normalized "
     <> toFixed 6 (normLongit) <> "°"
  log $  "Earth orbit eccentrity " <> toFixed 6 orbitEccentrity
  log $ "Sun eq. of center "
     <> toFixed 6 sunEquationCenter
  log $ "Sun true longitude "
    <> toFixed 5 sunTrueLongitudeNormal <> "°"
  log $ "Sun apparent longitude "
    <> toFixed 5 apparentLongitude <> "°"
  log $ "Apparent longitude normalized "
    <> toFixed 5 apparLongitNormal <> "°"
  log $ "Mean oblique ecliptic "
    <> toFixed 5 meanObliqueEcliptic <> "°"
  log $ "Corrected obliquity "
    <> toFixed 5 correctedOblique <> "°"
  log $ "Sun declination "
    <> toFixed 5 declinationSun <> "°"
  log $ "Variable Y = " <> toFixed 6 varY
  log $ "Time Equation " <> toFixed 5 timeEquation
    <> " minutes"
  log $ "Sunrise HA " <> toFixed 5 sunriseHourangle

-- expected 119.338928
normAnomal = modulo meanAnomal (fromNumber 360.0) :: Decimal

-- excepted 8682.677609
meanLongitude :: Decimal
meanLongitude = fromNumber (calcSunML cent2)

--  normalized expected 42,677609
normLongit = modulo meanLongitude (fromNumber 360.0) :: Decimal

-- excepted 0,016699
orbitEccentrity = fromNumber (eccentEarthOrbit cent2) :: Decimal

-- expected 1,6509793
sunEquationCenter = fromNumber (sunEqCntr cent2) :: Decimal

-- Sun true longitude, expected 44,32858
sunTrueLongitude = fromNumber (trueLongSun cent2) :: Decimal

sunTrueLongitudeNormal =
  modulo sunTrueLongitude (fromNumber 360.0) :: Decimal
  
apparentLongitude =
  fromNumber (appLongSun cent2) :: Decimal

-- expect 44.32024  
apparLongitNormal =
  modulo apparentLongitude (fromNumber 360.0) :: Decimal
  
-- Mean oblique ecliptic, expected 23,43626
meanObliqueEcliptic =
  fromNumber (meanObliqEclip cent2) :: Decimal

-- Corrected oblique, expect 23,43839
correctedOblique =
  fromNumber (obliqCorr cent2) :: Decimal
  
-- Sun declination
declinationSun =
  fromNumber (sunDeclination cent2) :: Decimal

-- variable Y, expected 0.043031
varY =
  fromNumber (variableY cent2) :: Decimal
  
-- Equation of time, expected 3.23768
timeEquation =
  fromNumber (equatTime cent2) :: Decimal
  
-- HA of sunrise, expected 133.01725

sunriseHourangle =  fromNumber sunriseHA :: Decimal

stringA :: Int -> Int -> Int -> String
stringA j m t =
  let preZero n = if n < 10 then "0" else ""
  in  show j <> "-" 
      <> preZero m <> show m <> "-"
      <> preZero t <> show t

stringB :: Int -> Int -> Int -> String
stringB j m t =
      " JDN = " <> show (jdnGr j m t)

printAB ::  Int -> Int -> Int -> String
printAB j m t =
  (stringA j m t) <> (stringB j m t)



-- JD + UTC time
jdateGr :: Int -> Int -> Int -> Int -> Int -> Int -> Decimal
jdateGr y m d hr mn sc =
    let
        jdn :: Int
        jdn =
            jdnGr y m d
    in
        fromInt jdn
        + fromInt ( hr - 12 )
        / fromInt 24
        + fromInt mn
        / fromInt 1440
        + fromInt sc
        / fromNumber 86400.0
        

cent :: Decimal
cent = getCent (jdateGr 2023 5 5 1 27 58)

cent2 = toNumber cent :: Number

getCent :: Decimal -> Decimal
getCent jd =
    let
        jD2000 = fromInt 2451545
    in
        (jd - jD2000 ) / fromNumber 36525.0


meanAnomal = fromNumber (meanAnomalSun cent2) :: Decimal

modDec :: Decimal -> Int -> String
modDec x b = 
        if x < fromInt b then toFixed 6 x
        else modDec (x - (fromInt b)) b
        
meanAnomalSun :: Number -> Number
meanAnomalSun cnt =
    let
        a = 357.52911 
        b = 35999.05029
        c = 1.537e-4
    in
        a + cnt * (b + cnt * c)



-- Mean Longitude of Sun

calcSunML :: Number -> Number
calcSunML cnt =
    let
        a = 280.46646
        b = 36000.76983
        c = 3.032e-4
    in
    a + cnt * (b + cnt * c) -- |> decNorm360
    
    
    -- Eccentricy of Earth Orbit

eccentEarthOrbit :: Number -> Number
eccentEarthOrbit cnt =
    0.016708634
        - cnt
        * (4.2037e-5 + 1.267e-7 * cnt)


-- Expected 1,6509793

sunEqCntr :: Number -> Number
sunEqCntr cnt =
  let anom = (meanAnomalSun cnt)
  in
    sinDeg (anom)
        * (1.914602 - cnt * (0.004817 + 0.000014 * cnt))
        + sinDeg (2.0 * anom)
        * (0.019993 - 0.000101 * cnt)
        + sinDeg (3.0 * anom)
        * 0.000289

-- Sun true Longitude

trueLongSun :: Number -> Number
trueLongSun cnt =
    calcSunML cnt + sunEqCntr cnt


-- Sun apparent longitude, OK tested 22.10.19

appLongSun :: Number -> Number
appLongSun cnt =
    (trueLongSun cnt) - 5.69e-3
    - 4.78e-3 * sinDeg (125.04 - 1934.136 * cnt)

-- Mean oblique ecliptic
meanObliqEclip :: Number -> Number
meanObliqEclip cnt =
    23.0 + (26.0 + (21.448 - cnt * (46.815 + cnt * (5.9e-4 - cnt * 1.813e-3))) / 60.0) / 60.0

-- Corrected Oblique
obliqCorr :: Number -> Number
obliqCorr cnt =
    (meanObliqEclip cnt)
    + 0.00256 * cosDeg (125.04 - 1934.136 * cnt)


-- Sun declination, expected 16.13515
sunDeclination :: Number -> Number
sunDeclination cnt =
    let 
        appLS = sinDeg (appLongSun cnt)
        oblC  = sinDeg (obliqCorr cnt)
    in
        asinDeg (oblC * appLS)

-- Variable y
variableY :: Number -> Number
variableY cnt =
    let
        x =
            tanDeg (obliqCorr cnt / 2.0)
    in
    x * x


-- Equation of Time

equatTime :: Number -> Number
equatTime cnt =
    let
        variaY =
            variableY cnt

        meanLongS =
            calcSunML cnt

        eOrbitEx =
            eccentEarthOrbit cnt

        meanAnomS =
            meanAnomalSun cnt
    in
    toDeg
        (variaY
            * sinDeg (2.0 * meanLongS)
            - 2.0
            * eOrbitEx
            * sinDeg meanAnomS
            + 4.0
            * eOrbitEx
            * variaY
            * sinDeg meanAnomS
            * cosDeg (2.0 * meanLongS)
            - 0.5
            * variaY
            * variaY
            * sinDeg (4.0 * meanLongS)
            - 1.25
            * eOrbitEx
            * eOrbitEx
            * sinDeg (2.0 * meanAnomS)
        )
        * 4.0


srHA :: Number -> Number -> Number
srHA cnt zenith =
    let
        geoLat =
            65.85

        declination =
            sunDeclination cnt

        x =
            cosDeg zenith
                / (cosDeg geoLat * cosDeg declination)
                - (tanDeg geoLat * tanDeg declination)
    in
    if x > 0.99999 && declination < 0.0 then
        0.0

    else if x < -0.99999 && declination > 0.0 then
        180.0

    else
        acosDeg x



sunriseHA = srHA cent2 90.833 :: Number -- zenith at sunrise



-- Civil twilight Sunrise HA


-- conversion radians to degrees
toDeg :: Number -> Number
toDeg =
    \beta -> 180.0 * beta / pi

asinDeg :: Number -> Number
asinDeg =
    \x -> toDeg (asin x)

sinDeg :: Number -> Number
sinDeg x = sin (pi*x/180.0) 


cosDeg :: Number -> Number
cosDeg x = cos (pi*x/180.0)


acosDeg :: Number -> Number
acosDeg =
    \x -> toDeg (acos x)
    

tanDeg :: Number -> Number
tanDeg x = tan (pi*x/180.0)
