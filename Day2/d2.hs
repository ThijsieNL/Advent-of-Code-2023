import Data.List.Split
import Data.Char (digitToInt)


main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    let color = emptyColor
    let inputArray = lines fileContent
    let splittedOnSemicolon = map (splitOn "; ") inputArray 
    let allColors = map (sumColors . makeAllColors emptyColor) splittedOnSemicolon
    let validColors = checkValidColor allColors
    print $ sumIDs validColors 1

-- add up the ID's of valid games
-- 12 red cubes, 13 green cubes, and 14 blue cubes
-- each game gets a color count. then we can compare said color to the required values
-- 1 entry of 5 sets: ["1 red, 3 blue, 11 green"," 1 blue, 5 red"," 3 blue, 5 green, 13 red"," 6 red, 1 blue, 4 green"," 16 red, 12 green"]
data Color = Color{red :: Int, green :: Int , blue :: Int} deriving Show
emptyColor :: Color
emptyColor = Color 0 0 0

addColors :: Color -> Color -> Color
addColors (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)

sumColors :: [Color] -> Color
sumColors = foldr addColors (Color 0 0 0)


checkValidColor :: [Color] -> [Bool]
checkValidColor = map (\x -> red x < 13 && green x < 14 && blue x < 15)
{- same semantics, just haskell reduce
checkValidColor [] = []
checkValidColor (x:xs) = (red x < 13 && green x < 14 && blue x < 15) : checkValidColor xs
-}

sumIDs :: [Bool] -> Int -> Int
sumIDs [] _ = 0
sumIDs (x:xs) id = if x then id + sumIDs xs (id+1) else sumIDs xs (id+1)

sumIndexesOfTrue :: [Bool] -> Int
sumIndexesOfTrue boolList = sum [index | (value, index) <- zip boolList [1..], value]

-- given a color and a character array, make for each encountered color
-- given a single set, like "1 red, 3 blue, 11 green", return a new color that has the added amounts
-- map dit over 1 entry
makeAllColors :: Color -> [String] -> [Color]
makeAllColors _ [] = []
makeAllColors color (x:xs) = addToColor color x : makeAllColors color xs

addToColor :: Color -> String -> Color
addToColor color str = makeColor color scores
    where
        a@(x:xs) = splitOn ", " str -- ["1 red", "3 blue", "11 green"]
        scores = map checkAmount a --scores: [(1,"red"),(3,"blue"),(11,"green")]
    --return $ makeColor color scores 


-- let OneEntryColor = map checkamount (1 entry of 5 sets) -> all entries are splittedOnSemicolon
-- this gives a list of (int,string)-combinations. then map makeColor over this
--convert a 
checkAmount :: String -> (Int, String)
checkAmount str
    | 'g' `elem` str = (read (head $ splitOn " " str), "green")
    | 'b' `elem` str = (read (head $ splitOn " " str), "blue")
    | otherwise = (read (head $ splitOn " " str), "red")

--read the string and the int value, 
-- map makeColor (Array of )
makeColor ::  Color -> [(Int, String)] -> Color
makeColor c [] = c
makeColor  Color{red = r, green = g, blue = b } (x:xs) =
    case snd x of
        "red"  -> makeColor (Color (r + fst x) g b) xs
        "blue" -> makeColor (Color r g (b + fst x)) xs
        "green" -> makeColor (Color r (g + fst x) b) xs