data Shape = Circle Float
           | Rectangle Float Float
           | Triangle Float Float Float
           deriving (Show)

class ShapeClass a where
    area' :: a -> Float

circleArea :: Float -> Float
circleArea r = pi * r * r

rectangleArea :: Float -> Float -> Float
rectangleArea l w = l * w

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

instance ShapeClass Shape where
    area' (Circle r) = circleArea r
    area' (Rectangle l w) = rectangleArea l w
    area' (Triangle a b c) = triangleArea a b c

main :: IO ()
main = do
    let shapes = [Circle 5, Rectangle 4 6, Triangle 3 4 5]
    mapM_ printShapeArea shapes

printShapeArea :: Shape -> IO ()
printShapeArea shape = putStrLn $ "The area of " ++ show shape ++ " is " ++ show (area' shape)
