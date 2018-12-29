{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as VM

-- ex 1
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- ex 2
parseExpT :: String -> Maybe ExprT
parseExpT = parseExp Lit (Add) (Mul)

evalStr :: String -> Maybe Integer
evalStr = (fmap eval) . parseExpT

--ex 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

--ex 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a = if a <= 0 then False else True
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x+y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x*y) 7)

-- tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7
-- cool!

-- ex 5
instance Expr VM.Program where
  lit x = [(VM.PushI x)]
  add xs ys = xs ++ ys ++ [VM.Add]
  mul xs ys = xs ++ ys ++ [VM.Mul]

testPrg = testExp :: Maybe VM.Program

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
