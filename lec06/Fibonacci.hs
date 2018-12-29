{-# LANGUAGE LambdaCase #-}
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

-- ex 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList = unfoldr (\case Cons a as -> Just (a, as))

instance Show a => Show (Stream a) where
  show = (\x -> x ++ ", ...") . show . (take 10) . streamToList

-- ex 4
--  (a -> b -> b) -> b -> t a -> b

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- ex 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0
