-- Define the binary search tree data structure
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Function to insert an element into the BST
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node a left right)
    | x == a = Node a left right  -- Element already exists, do nothing
    | x < a  = Node a (insert x left) right  -- Insert into the left subtree
    | x > a  = Node a left (insert x right)  -- Insert into the right subtree

-- Function to check if an element is in the BST
search :: (Ord a) => a -> Tree a -> Bool
search x Empty = False
search x (Node a left right)
    | x == a = True
    | x < a  = search x left
    | x > a  = search x right

-- Function to display the BST in order (in-order traversal)
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a left right) = inOrder left ++ [a] ++ inOrder right

-- Example usage
main :: IO ()
main = do
    let tree = foldr insert Empty [8, 3, 10, 1, 6, 14, 4, 7, 13]
    putStrLn "Binary Search Tree:"
    print tree
    putStrLn "In-order Traversal:"
    print (inOrder tree)
    putStrLn "Search for 6 in the tree:"
    print (search 6 tree)
    putStrLn "Search for 15 in the tree:"
    print (search 15 tree)

-- The `main` function demonstrates the use of the `insert`, `search`, and `inOrder` functions.
