module Solar.Current.Main where

import Prelude
import Data.DateTime (year, month, day, hour, minute, second)
import Data.Enum (fromEnum)
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDate, nowTime)
import Data.Int (toNumber)
import Data.Number (pow, sin, cos, asin, acos, tan, pi, floor, round, remainder)
import Data.Number.Format (toString, toStringWith, fixed)
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

getDate :: Effect Unit
getDate  = do
  currDate <- nowDate
  let
    currYear = fromEnum $ year currDate :: Int
    currMonth = fromEnum $ month currDate :: Int
    currDay = fromEnum $ day currDate :: Int
    
  currentTime <- nowTime
  let
    cHour = fromEnum $ hour currentTime :: Int
    cMinute = fromEnum $ minute currentTime :: Int
    cSecond = fromEnum $ second currentTime :: Int
    cent1 = getCent (jdateGr currYear currMonth currDay cHour cMinute cSecond) :: Number
    declinationSun = toStringWith (fixed 5) (sunDeclination cent1) :: String
    timeEquat = toStringWith (fixed 5) (equatTime cent1) :: String
    sunriseHA = srHA cent1 90.833 :: Number -- zenith at sunrise
    minutesNoon = getNoon cent1 24.18 2.0 :: Number
    sunriseTornio = sunRise cent1 24.18 2.0 :: Number
    sunsetTornio  = sunSet cent1 24.18 2.0 :: Number
    dayLength = sunriseHA * 8.0 :: Number
    trueSolarTime = trueSolTime cent1 cHour cMinute cSecond 2.0 24.18 :: Number
    

  log $  "Current Julian day " <> (stringA currYear currMonth currDay )
    <> " " <> show cHour <> ":" <> show cMinute <> ":" <> show cSecond
    <> " JD = " 
    <>  toStringWith (fixed 6) (jdateGr currYear currMonth currDay cHour cMinute cSecond)
    <> "\nCentury " <> toStringWith (fixed 9) cent1
    <> "\nSun declination " <> declinationSun <> "°"
    <> "\nTime Equation " <> timeEquat <> " minutes"
    <> "\nSunrise HA " <> (toStringWith (fixed 5) sunriseHA)
    <> "\nNoon time " <>  mnsToHrMnSc minutesNoon
    <> "\nSunrise time " <> mnsToHrMnSc sunriseTornio
    <> "\nSunset time " <> mnsToHrMnSc sunsetTornio
    <> "\nDaylength " <> mnsToHrMnSc dayLength
    <> "\nTrue solar time "
    <> toStringWith (fixed 4) trueSolarTime


main :: Effect Unit
main  =
  render =<< withConsole do
  log $  "SOLAR CALCULATOR"
 
  getDate


  log $ "Hour angle 2 " <> toString hourAngle2
  log $ "Solar Zenith "
    <> toStringWith (fixed 4) solarZenith <> "°"
  log $ "Solar elevation "
    <> toStringWith (fixed 4) solarElevation <> "°"
  log $ "Atmospheric refraction "
    <> toStringWith (fixed 6) atmosphericRefraction
  log $ "Refraction-corrected altitude "
    <> toStringWith (fixed 6) refractCorrectedAltitude <> "°"
  log $ "Solar azimuth " <> toStringWith (fixed 5) solarAzimuth <> "°"

-- expected 119.338928
normAnomal = decmod meanAnomal :: Number

-- excepted 8682.677609

meanLongitude = calcSunML cent2 :: Number

--  normalized expected 42,677609
normLongit = decmod meanLongitude :: Number

-- excepted 0,016699
orbitEccentrity =
  toStringWith (fixed 6) (eccentEarthOrbit cent2) :: String

-- expected 1,6509793
sunEquationCenter = toStringWith (fixed 6) (sunEqCntr cent2) :: String

-- Sun true longitude, expected 44,32858
sunTrueLongitude = trueLongSun cent2 :: Number

sunTrueLongitudeNormal =
  decmod sunTrueLongitude :: Number
  
apparentLongitude =
  appLongSun cent2 :: Number

-- expect 44.32024  
apparLongitNormal =
  decmod apparentLongitude :: Number
  
-- Mean oblique ecliptic, expected 23,43626
meanObliqueEcliptic =
  meanObliqEclip cent2 :: Number

-- Corrected oblique, expect 23,43839
correctedOblique =
  toStringWith (fixed 5) (obliqCorr cent2) :: String
  
-- Sun declination
-- declinationSun =
--  toStringWith (fixed 5) (sunDeclination cent) :: String

-- variable Y, expected 0.043031
--varY =
--  toStringWith (fixed 6) (variableY cent2) :: String
  
-- Equation of time, expected 3.23768
timeEquation =
  toStringWith (fixed 5) (equatTime cent2) :: String
  
-- HA of sunrise, expected 133.01725
--srHourAngle =
--  toStringWith (fixed 5) (sunriseHA) :: String

-- Daylength in minutes, expected 1064.14 


-- True solar time, expected 187.9477 (elm 187.9243526802112)
  

-- Hour angle, expected -133,01308
hourAngle2 =
  hourAngle cent2 10 20 1 2.0 24.18 :: Number

-- Solar Zenith (degrees),expected 90.8318
solarZenith =
  solZenith 65.85 cent2 :: Number

-- Sun altitude at the time set, exception -0.8318
solarElevation = 90.0 - solarZenith :: Number

-- Atmospheric refraction, expected 0.397418

atmosphericRefraction =
  atmosRefract 65.85 cent2 :: Number

-- Refraction corrected altitude, expected -0.43438
refractCorrectedAltitude =
  refractCorrectAltitude 65.85 cent2 :: Number

-- Solar azimuth, expected 44.62545

solarAzimuth = solAzimuth 65.85 cent2 10 20 1 2.0 24.18 :: Number


solZenith :: Number -> Number -> Number
solZenith lat cnt =
    let
        b3 = lat

        t2 =
            sunDeclination cnt
    in
    acosDeg (sinDeg b3 * sinDeg t2 + cosDeg b3 * cosDeg t2 * cosDeg hourAngle2)



-- Atmospheric Refraction

atmosRefract :: Number -> Number -> Number
atmosRefract lat cnt =
    let
        solElev =
            90.0 - (solZenith lat cnt)
    in
    if solElev > 85.0 then
        0.0

    else if solElev > 5.0 then
        (58.1
            / tanDeg solElev
            - 0.07
            / pow (tanDeg solElev) 3.0
            + 8.6e-5
            / pow (tanDeg solElev) 5.0
        )
            / 3600.0

    else if solElev > -0.575 then
        (1735.0
            + solElev
            * (-518.2
                + solElev
                * (103.4 + solElev * (-12.79 + solElev * 0.711))
              )
        )
            / 3600.0

    else
        -20.772 / tanDeg solElev / 3600.0



refractCorrectAltitude :: Number -> Number -> Number
refractCorrectAltitude lat cnt =
    90.0
        - solZenith lat cnt
        + atmosRefract lat cnt



--  Solar Azimuth angle clockwise from north

preAzimuth :: Number -> Number -> Number
preAzimuth lat cnt  =
    let
        b3 =
            lat

        ad =
            solZenith lat cnt

        t =
            sunDeclination cnt
    in
    acosDeg ((sinDeg b3 * cosDeg ad - sinDeg t) / (cosDeg b3 * sinDeg ad))


solAzimuth :: Number -> Number -> Int -> Int -> Int -> Number -> Number -> Number
solAzimuth lat cnt hr mn sc tz longit =
    let
        preAz =
            preAzimuth lat cnt

        ac =
            hourAngle  cnt hr mn sc tz longit
    in
    if ac > 0.0 then
        decmod (preAz + 180.0)

    else
        decmod (540.0 - preAz)



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
jdateGr :: Int -> Int -> Int -> Int -> Int -> Int -> Number
jdateGr y m d hr mn sc =
    let
        jdn :: Int
        jdn =
            jdnGr y m d
    in
        toNumber jdn
        + toNumber ( hr - 12 )
        / toNumber 24
        + toNumber mn
        / toNumber 1440
        + toNumber sc
        / 86400.0
        

cent :: Number
cent = getCent (jdateGr 2923 5 5 10 20 1)

cent2 =  cent :: Number

getCent :: Number -> Number
getCent jd =
    let
        jD2000 = toNumber 2451545
    in
        (jd - jD2000 ) / 36525.0


meanAnomal = (meanAnomalSun cent2) :: Number

        
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


-- Sun apparent longitude

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



-- sunriseHA = srHA cent2 90.833 :: Number -- zenith at sunrise


-- Noon time as minutes since midnight

getNoon :: Number -> Number -> Number -> Number
getNoon cnt geoLong timeZone =
    let
        eqTime =
            equatTime cnt
    in
    (720.0 - 4.0 * geoLong) - eqTime + timeZone * 60.0



-- Sunrise in minutes, option = -1
sunRise :: Number -> Number -> Number -> Number
sunRise cnt geoLong timeZone =
    risetMns cnt geoLong timeZone false


-- Sunset  in minutes, option = +1
sunSet :: Number -> Number -> Number -> Number
sunSet cnt geoLong timeZone =
    risetMns cnt geoLong timeZone true

risetMns :: Number -> Number -> Number -> Boolean -> Number
risetMns  cnt geoLong timeZone rsOption =
    let noon = getNoon cnt geoLong timeZone
        getHA = srHA cnt 90.833 :: Number
        c = if rsOption then 1.0
            else -1.0
    in  noon + 4.0 * c * getHA


-- True Solar Time

trueSolTime :: Number -> Int -> Int -> Int -> Number -> Number -> Number
trueSolTime cnt hr mn sc tz longit =
    let
        e2 =
            60.0 * ((toNumber hr) + tz)
            + (toNumber mn)
            + (toNumber sc) / 60.0 :: Number

        v2 =
            equatTime cnt :: Number
    in
        e2 + v2 + 4.0 * longit - 60.0 * tz


-- Hour Angle degr

hourAngle :: Number -> Int -> Int -> Int -> Number -> Number -> Number
hourAngle cnt hr mn sc tz longit =
    let
        tSt =
            trueSolTime  cnt hr mn sc tz longit
    in
    if tSt > 0.0 then
        0.25 * tSt - 180.0

    else
        0.25 * tSt + 180.0


-- Normalize angles to max 360 grades
-- replaces Decimal.modulo

decmod :: Number -> Number
decmod x =
  if x < 360.0 then x
  else decmod (x - 360.0)

-- Converts minutes to hh:mm:ss
-- also decimal part to seconds,
-- example mnToHrMn 1435.25 -> 23:55:15

mnsToHrMnSc :: Number -> String
mnsToHrMnSc mns =
  let
    pre0 t = if t < 10.0 then "0" else ""
    hr = floor (mns / 60.0)
    hr0 = pre0 hr
    mn = floor (mns - 60.0 * hr)
    mn0 = pre0 mn
    sc = round (remainder (mns * 60.0) 60.0)
    sc0 = pre0 sc
  in
    hr0 <> toString hr <> ":" <> mn0 <> toString mn <> ":"
    <> sc0 <> toString sc



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

