import Data.List.Split
import Data.Char (digitToInt)


main :: IO ()
main = do
    fileContent <- readFile "inp.txt"
    --this is all lines in the file as one array entry.
    let color = emptyColor
    let inputArray = lines fileContent
    let splittedOnSemicolon =  map (splitOn ";") inputArray -- :t [[String]] == [[[Char]]]
    let c = addToColor color "5 red, 3 blue, 11 green" 
    let resColor = map (addToColor color) $ head splittedOnSemicolon
    print c

-- 12 red cubes, 13 green cubes, and 14 blue cubes
-- each game gets a color count. then we can compare said color to the required values
-- 1 entry of 5 sets: ["1 red, 3 blue, 11 green"," 1 blue, 5 red"," 3 blue, 5 green, 13 red"," 6 red, 1 blue, 4 green"," 16 red, 12 green"]
data Color = Color{red :: Int, green :: Int , blue :: Int} deriving Show
emptyColor :: Color
emptyColor = Color 0 0 0

{-sum a list of colors-}
{-sum two colors-}

-- given a color and a character array, make for each encountered color
-- given a single set, like "1 red, 3 blue, 11 green", return a new color that has the added amounts
-- map dit over 1 entry
makeColorFromEntry :: [String] -> Color -> [Color]
makeColorFromEntry [] c = [c]
makeColorFromEntry str@(x:xs) c = map (addToColor c) str 

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
        "red"  -> makeColor (Color (r+1) g b) xs
        "blue" -> makeColor (Color r g (b+1)) xs
        "green" -> makeColor (Color r (g+1) b) xs
--Read one element, find out what the value is and to what color it matches