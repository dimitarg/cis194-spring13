-- Ex 1.1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = (foldr (\a b -> (a - 2) * b) 1) . (filter even)


-- Ex 1.2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

---- 2, fun2 1 = fun2 4 = 4, fun2 2 = 2, fun2 1 =

elemm :: Integer -> Integer
elemm x
 | even x = x
 | x == 1 = 0
 | otherwise = 3 * x + 1

series :: Integer -> [Integer]
series x = iterate (\x -> elemm x `div` 2) (elemm x)

fun2Pos :: Integer -> Integer
fun2Pos a = sum (takeWhile (>0) (filter (/=1) (series a)))

fun2' :: Integer -> Integer
fun2' x = if x > 0 then fun2Pos x else -(fun2Pos x)

-- ex 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert newNode  (Node h left oldNode right) =
  let shouldInsertLeft = (height left) < (height right)
      newLeft          = if shouldInsertLeft then insert newNode left else left
      newRight         = if shouldInsertLeft then right               else insert newNode right
      newHeight        = (max (height left) (height right)) + 1
  in Node newHeight newLeft oldNode newRight

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

-- test
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ left _ right) = abs ((height left) - (height right)) <= 1

-- ex 3.1
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False

-- ex 3.2
mapViaFold :: (a -> b) -> [a] -> [b]
mapViaFold f = foldr ( \x acc -> (f x):acc ) []

-- ex 3.3
foldlViaFoldr :: (a -> b -> a) -> a -> [b] -> a
foldlViaFoldr f z xs = foldr (flip f) z (reverse xs)
