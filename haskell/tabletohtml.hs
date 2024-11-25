import Text.Read.Lex (Number)
import Numeric

-- Generate 2D-list
-- row = height cm
-- col = weight kg
-- cell [row,column] [1,1]
-- bmi = weight[1] / height[1]^2

bmi r c  =
    let w = head results !! c
        h = head (results !! r)
    in  w / (h / 100)^2

-- Function to generate HTML table from the 2D list
generateHtmlTable :: [[Float]] -> String
generateHtmlTable table =
    "<html>\n<head>\n"
    <> "<title>BMI-index</title>\n"
    <> "<style>\n"
    <> "body {margin-left: 5%; font-family: Arial, Helvetica, sans-serif;}\n"
    <> "h1 {color: red;}\n"
    <> "table, td {border: 2px solid blue; border-collapse: collapse; font-size: 1.2em;}\n"
    <> "td {padding: 5px;}\n"
    <> "p {color: blue; font-size: 1.2em;}\n"
    <> "</style>\n"
    <> "</head>\n"
    <> "<body>\n"
    <> "<h1>BODY-MASS INDEX</h1>\n"
    <> "<table>\n" ++
    concatMap generateHtmlRow table ++
    "</table>"

-- Function to generate HTML for a single row
generateHtmlRow :: [Float] -> String
generateHtmlRow row =
    "  <tr >\n" ++
    concatMap generateHtmlCell row ++
    "  </tr>\n"

-- Function to generate HTML for a single cell
generateHtmlCell :: Float -> String
generateHtmlCell cell =
    "  <td>" ++ formD cell ++ "</td>\n"


results =
    [[0,60,65,70,75,80,85]
    ,[160,bmi 1 1,bmi 1 2,bmi 1 3,bmi 1 4,bmi 1 5,bmi 1 6]
    ,[165,bmi 2 1,bmi 2 2,bmi 2 3,bmi 2 4,bmi 2 5,bmi 2 6]
    ,[170,bmi 3 1,bmi 3 2,bmi 3 3,bmi 3 4,bmi 3 5,bmi 3 6]
    ,[175,bmi 4 1,bmi 4 2,bmi 4 3,bmi 4 4,bmi 4 5,bmi 4 6]
    ,[180,bmi 5 1,bmi 5 2,bmi 5 3,bmi 5 4,bmi 5 5,bmi 5 6]
    ,[185,bmi 6 1,bmi 6 2,bmi 6 3,bmi 6 4,bmi 6 5,bmi 6 6]
    ]


formD v = showFFloat (Just 2) v ""

-- Main function to display the HTML table
main :: IO ()
main = do
    let wmin = head results !! 1; wmax = head results !! 6 -- Weight [kg]
    let hmin = head (results !! 1); hmax = head (results !! 6) -- Height [cm]
    let legend1 = "<p>Weights " ++ show wmin ++ " - " ++ show wmax ++ " kg</p>"
    let legend2 = "<p>Heights " ++ show hmin ++ " - " ++ show hmax ++ " cm</p>"
    let htmlTable = generateHtmlTable results ++ legend1 ++ legend2 ++ "</body>"
    putStrLn htmlTable
