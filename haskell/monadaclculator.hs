module Main where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)

-- Define a simple eval function for basic arithmetic expressions
eval :: String -> Maybe Double
eval expr = parseExpr (words expr)

parseExpr :: [String] -> Maybe Double
parseExpr [x] = readMaybe x
parseExpr (x:op:y:rest) = do
    a <- readMaybe x
    b <- readMaybe y
    result <- applyOp op a b
    parseExpr (show result : rest)
parseExpr _ = Nothing

applyOp :: String -> Double -> Double -> Maybe Double
applyOp "+" = Just .: (+)
applyOp "-" = Just .: (-)
applyOp "*" = Just .: (*)
applyOp "/" = \x y -> if y == 0 then Nothing else Just (x / y)
applyOp _   = const (const Nothing)

-- Helper function to apply a binary operator
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
main :: IO ()
main = do
    putStrLn "Enter expression (e.g., 1 + 2):"
    expression <- getLine
    let result = eval expression
    putStrLn $ "Result: " ++ show result