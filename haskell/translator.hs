import Data.Text (pack,unpack,replace)
import Data.Text.Internal (Text)

original :: String
original = "Viimeisin säähavainto : \
\ Su 1.12. 14:50 \n \
\ Lämpötila  0,2 ℃\n \ 
\ Kastepiste 0,1 ℃\n \
\ Kosteus   99 % \n \
\ Tuuli      1 m/s \n \
\ Tuulen suunta \n \
\ kaakkoistuulta (150°)" 

{-
Tuulen puuska
2 m/s
Sade
0,0 mm (13:00-14:00)
Näkyvyys
12 km
Pilvisyys
Pilvistä (8/8)
Paine
1002,2 hPa"
-}

lastWord :: Text
lastWord = (translate "kaakkoistuulta" "southeast wind"
 . translate "Tuulen suunta" "Wind direction"
 . translate "Tuuli" "Wind"
 . translate "Kosteus" "Humidity"
 . translate "Kastepiste" "Dev point"
 . translate "Lämpötila" "Temperature"
 . translate "säähavainto" "weather observation"
 . translate "Viimeisin" "The latest" . pack) original


translate :: String -> String -> Text -> Text
translate old new = replace (pack old) (pack new)


main :: IO ()
main = do
  putStrLn original
  putStrLn "_________________\f"
  putStrLn ( unpack lastWord )
