data Shape = Circle Float | Rectangle Float Float | Triangle Float Float

class ShapeClass a where
    area' :: a -> Float


instance ShapeClass Shape where
    area' (Circle r) = pi * r * r
    area' (Rectangle w h) = w * h
    area' (Triangle b h) = 0.5 * b * h

main = do
    let circleArea = area' (Circle 5.0) -- Area of a circle with radius 5.0
    let rectangleArea = area' (Rectangle 4.0 6.0) -- Area of a rectangle with width 4.0 and height 6.0
    let triangleArea = area' (Triangle 3.0 4.0) -- Area of a triangle with base 3.0 and height 4.0
    putStrLn $ "Area of Circle: " ++ show circleArea
    putStrLn $ "Area of Rectangle: " ++ show rectangleArea
    putStrLn $ "Area of Triangle: " ++ show triangleArea