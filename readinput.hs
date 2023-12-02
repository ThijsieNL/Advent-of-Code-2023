main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    let inputArray = lines fileContent
    return undefined