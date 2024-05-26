import System.IO
import Data.Char (toLower)
import Data.List (sort, group)

-- Function to count word occurrences in a file
main :: IO ()
main = do
    contents <- readFile "test.txt"
    let wordsList = words $ map toLower contents
        wordGroups = group $ sort wordsList
        counts = map (\ws -> (head ws, length ws)) wordGroups
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts
