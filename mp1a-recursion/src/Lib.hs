--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake
-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = [] -- base case: empty list
mytake n _
    | n <= 0 = [] -- base case: index is non-positive
mytake n (x:xs) = x : mytake (n - 1) xs -- recursive case


--- ### mydrop
-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = [] -- base case: empty list
mydrop n xs
    | n <= 0 = xs
mydrop n (_:xs) = mydrop (n - 1) xs -- recursive case

--- ### rev
-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev xs = go xs []
    where
        go [] acc = acc
        go (x:xs) acc = go xs (x:acc) -- put elements at front of accumulator

--- ### app
-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] xs = xs
app (x:xs) ys = x : app xs ys -- recurse on the first list

--- ### inclist
-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = x + 1 : inclist (xs)

--- ### sumlist
-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist(xs)

--- ### myzip
-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] xs = []
myzip xs [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs
-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
-- addpairs xs [] = []
-- addpairs [] xs = []
addpairs xs ys = addpairs' (myzip xs ys)
    where
        addpairs' [] = []
        addpairs' ((a,b):rest) = (a + b) : addpairs' rest

--- ### ones
-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats
-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib
-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
-- tail :: [a] -> [a]
-- fib' 0 = 0
-- fib' 1 = 1
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add
-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add n [] = [n]
add n (x:xs)
    | n < x = n:x:xs
    | n == x = x:xs -- already present, no change
    | otherwise = x : add n xs

--- ### union
-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
-- union [] [] = []
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys)
    | x < y = x : (union xs (y:ys))
    | x > y = y : (union (x:xs) ys)
    | otherwise = x : (union xs ys) -- just keep one if x == y

--- ### intersect
-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a] 
intersect _ [] = []
intersect [] _ = []
intersect (x:xs) (y:ys)
    | x < y = intersect xs (y:ys)
    | x > y = intersect (x:xs) ys
    | otherwise = x : intersect xs ys -- just keep one if x == y

--- ### powerset
-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
-- if we have a set (x:xs), then every subset either DOES include x or does NOT include x
powerset (x:xs) = 
    let ps = powerset xs
    in union ps (P.map (add x) ps)

--- Higher Order Functions
--- ----------------------

--- ### inclist'
-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' xs = P.map (+1) xs

--- ### sumlist'
-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' xs = P.foldr (+) 0 xs