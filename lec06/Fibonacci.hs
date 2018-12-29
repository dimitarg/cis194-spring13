import Data.List

-- ex 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = (fib (x-2)) + (fib (x-1))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- ex 2
fibs2' :: [Integer]
fibs2' = let
  gen :: Integer -> Integer -> [Integer]
  gen prev next = prev:(gen next (prev + next))
  in gen 0 1

fibs2 :: [Integer]
--unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
fibs2 = unfoldr (\(x, y) -> Just (x, (y, (x + y)))) (0, 1)
