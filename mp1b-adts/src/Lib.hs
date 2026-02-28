--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons
-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list
-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x y) = x : cons2list y

--- ### eval
-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp n) = n
eval (PlusExp []) = 0
eval (PlusExp xs) = sum (map eval xs)
eval (MultExp []) = 1
eval (MultExp ys) = product (map eval ys)

--- ### list2cons'
-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
-- list2cons' [] = Nil
list2cons' xs = foldr Cons Nil xs

--- ### BinTree
-- BinTree
-- Node :: a -> BinTree a -> BinTree a -> BinTree a
-- Leaf :: BinTree a
data BinTree a = Node a (BinTree a) (BinTree a)
        | Leaf
    deriving (Show, Eq)

--- ### sumTree
-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node a b c) = a + sumTree b + sumTree c

--- ### SimpVal
-- SimpVal
data SimpVal = IntVal Integer
        | BoolVal Bool
        | StrVal String
        | ExnVal String
        | None
    deriving (Show, Eq)

--- ### liftIntOp
-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal x) (IntVal y) = IntVal (op x y) -- perform an operation (op) on 2 Integers
liftIntOp _ _ _ = ExnVal "not an IntVal!" -- if the inputs are not IntVal