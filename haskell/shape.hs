data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float

area :: Shape -> Float

area (Circle r)=pi*r*r
area (Rectangle l w) =l*2
area (Triangle a b c) = let s = (a + b + c) / 2 in sqrt (s * (s - a) * (s - b) * (s - c))

circleShape = Circle 5.0
rectangleShape = Rectangle 4.0 6.0
triangleShape = Triangle 3.0 4.0 5.0

circleArea = area circleShape
rectangleArea = area rectangleShape
triangleArea = area triangleShape


main :: IO ()
main = do
	print(circleArea)
	print(rectangleArea)
	print(triangleArea)
