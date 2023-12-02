
import System.IO
import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex, isPrefixOf, findIndex)
import Data.Text (replace)
import Data.Maybe (fromMaybe)


main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    let inputArray = lines fileContent
    let filteredinput = map getnums inputArray
    let properInts = map chArrToInt filteredinput
    let allInts = map combineFirstAndLast properInts

    print $ sum allInts

nums = ['0','1', '2', '3', '4','5','6','7','8','9']

combineFirstAndLast :: [Int] -> Int
combineFirstAndLast arr = makeInt $ head arr : [last arr]

--getNums :: String -> String
getnums = filter isDigit
numAsWord = ["zero","one", "two", "tree", "four","five","six","seven","eight","nine"]


chArrToInt :: [Char] -> [Int]
chArrToInt [] = []
chArrToInt (x:xs)
    | x `elem` nums = digitToInt x : chArrToInt xs
    | otherwise =  chArrToInt xs

makeInt :: [Int] -> Int
makeInt [] = 0
makeInt arr = read . concatMap show $ arr


