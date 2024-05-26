import System.IO

-- Corrected type signature and function definition
printFileContents :: [FilePath] -> IO ()
printFileContents = mapM_ (\filePath -> readFile filePath >>= putStrLn)

main :: IO ()
main = printFileContents ["test.txt"]
