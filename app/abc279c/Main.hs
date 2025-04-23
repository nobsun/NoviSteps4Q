{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import Data.ByteString.Char8 qualified as B
import Data.Maybe
import Data.Ord

import Control.Arrow
import Control.Applicative
import Data.Array
import Data.Bool
import Data.Char
import Data.Function
import Data.List
import Text.Printf

import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V

import Debug.Trace qualified as Debug

debug :: Bool
debug = () /= ()

type I = String
type O = String

type Dom   = ([I],[I])
type Codom = O

type Solver = Dom -> Codom

solve :: Solver
solve = \ case
    (bss,css) -> bool "No" "Yes" (sort (transpose bss) == sort (transpose css))

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    [h,_]:rss -> case f $ splitAt (read h) (concat rss) of
        r -> [[r]]
    _   -> error "wrap: invalid input format"

main :: IO ()
main = B.interact (encode . wrap solve . decode)

class InterfaceForOJS a where
    readB :: B.ByteString -> a
    readBs :: B.ByteString -> [a]
    readBs = map readB . B.words
    decode :: B.ByteString -> [[a]]
    decode = map readBs . B.lines

    showB :: a -> B.ByteString
    showBs :: [a] -> B.ByteString
    showBs = B.unwords . map showB
    encode :: [[a]] -> B.ByteString
    encode = B.unlines . map showBs

instance InterfaceForOJS B.ByteString where
    readB = id
    showB = id

instance InterfaceForOJS Int where
    readB = readInt
    showB = showInt

instance InterfaceForOJS Integer where
    readB = readInteger
    showB = showInteger

instance InterfaceForOJS String where
    readB = readStr
    showB = showStr

instance InterfaceForOJS Double where
    readB = readDbl
    showB = showDbl

instance InterfaceForOJS Char where
    readB = B.head
    showB = B.singleton
    readBs = B.unpack
    showBs = B.pack

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

showInt :: Int -> B.ByteString
showInt = B.pack . show

readInteger :: B.ByteString -> Integer
readInteger = fst . fromJust . B.readInteger

showInteger :: Integer -> B.ByteString
showInteger = B.pack . show

readStr :: B.ByteString -> String
readStr = B.unpack

showStr :: String -> B.ByteString
showStr = B.pack

readDbl :: B.ByteString -> Double
readDbl = read . B.unpack

showDbl :: Double -> B.ByteString
showDbl = B.pack . show

{- Bonsai -}

{- debug -}
trace :: String -> a -> a
trace | debug     = Debug.trace
      | otherwise = const id

tracing :: Show a => a -> a
tracing = trace . show <*> id

{- error -}
impossible :: a
impossible = error "impossible"

invalid :: a
invalid = error "invalid input"

{- | 切り上げ除算
>>> 7 `ceilingDiv` 3
3
>>> 6 `ceilingDiv` 3
2
-}
ceilingDiv :: Integral a => a -> a -> a
ceilingDiv x y = negate $ negate x `div` y

{- |
>>> combinations 2 "abcd"
["ab","ac","ad","bc","bd","cd"]
-}
combinations :: Int -> [a] -> [[a]]
combinations = \ case
    0   -> const [[]]
    n+1 -> \ case 
        []   -> []
        x:xs -> map (x:) (combinations n xs) ++ combinations (n+1) xs
    _ -> error "negative"

nCr :: Integral a => a -> a -> a
nCr n r
    | n < 0  || r < 0  || n < r  = 0
    | n == 0 || r == 0 || n == r = 1
    | otherwise                  = iter 1 n 1
    where
        r' = min r (n-r)
        iter p m = \ case
            q | q > r'    -> p
              | otherwise -> iter (p * m `div` q) (pred m) (succ q)

nPr :: Integral a => a -> a -> a
nPr n r = product (genericTake r [n, pred n .. 1])

{- |
>>> spanCount odd [3,1,4,1,5,9]
(2,[4,1,5,9])
-}
spanCount :: (a -> Bool) -> [a] -> (Int, [a])
spanCount p = \ case
    []   -> (0,[])
    aas@(a:as)
        | p a       -> case spanCount p as of
            (c,bs)      -> (succ c, bs)
        | otherwise -> (0,aas)

{- |
>>> runLength "aaaabbbcddeeeeeefghhhh"
[('a',4),('b',3),('c',1),('d',2),('e',6),('f',1),('g',1),('h',4)]
-}
runLength :: Eq a => [a] -> [(a, Int)]
runLength = runLengthBy (==)

runLengthBy :: (a -> a -> Bool) -> [a] -> [(a, Int)]
runLengthBy eq = unfoldr phi
  where
    phi []     = Nothing
    phi (x:xs) = case spanCount (x `eq`) xs of
      (m, zs) -> Just ((x, succ m) , zs)

{- |
>>> splitEvery 3 [0 .. 10]
[[0,1,2],[3,4,5],[6,7,8],[9,10]]
-}
splitEvery :: Int -> [a] -> [[a]]
splitEvery k = \ case
    [] -> []
    xs -> case splitAt k xs of
        (ys,zs) -> ys : splitEvery k zs

{- |
>>> splice 5 "abcdefghij"
["abcde","bcdef","cdefg","defgh","efghi","fghij"]
-}
splice :: Int -> [a] -> [[a]]
splice n = (!! n) . transpose . map inits . tails

{- |
>>> subsegments "yay"
[["y","a","y"],["ya","ay"],["yay"]]
-}
subsegments :: [a] -> [[[a]]]
subsegments = drop 1 . transpose . map inits . transpose . tails 

{- |
>>> mex [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6]
15
-}
mex     ::  [Int] -> Int
mex xs  =   minform 0 (length xs, xs)

minform         ::  Int -> (Int, [Int]) -> Int
minform a (n,xs)
  | n == 0      =   a
  | m == b - a  =   minform b (n-m, vs)
  | otherwise   =   minform a (m, us)
    where  (us,vs)  =  partition (< b) xs
           b        =  a + 1 + n `div` 2
           m        = length us

{- misc -}
toTuple :: [a] -> (a,a)
toTuple = \ case
    x:y:_ -> (x,y)
    _     -> invalid

fromTuple :: (a,a) -> [a]
fromTuple (x,y) = [x,y]

countif :: (a -> Bool) -> [a] -> Int
countif = iter 0
    where
        iter a p (x:xs) = iter (bool a (succ a) (p x)) p xs
        iter a _ []     = a

divids :: [a] -> [([a],[a])]
divids = \ case
    []         -> [([],[])]
    xxs@(x:xs) -> ([], xxs) : map (first (x:)) (divids xs)

{- Sized List -}
data SzL a = SzL Int [a] deriving Eq

instance Ord a => Ord (SzL a) where
    compare :: Ord a => SzL a -> SzL a -> Ordering
    compare (SzL m xs) (SzL n ys) = case compare m n of
        EQ -> compare xs ys
        o  -> o

nilsz :: SzL a
nilsz = SzL 0 []

consz :: a -> SzL a -> SzL a
consz x (SzL m xs) = SzL (succ m) (x:xs)

singletonsz :: a -> SzL a
singletonsz = SzL 1 . singleton

szl :: b -> (a -> SzL a -> b) -> SzL a -> b
szl e f = \ case
    SzL _ []     -> e
    SzL m (x:xs) -> f x (SzL (pred m) xs)

append :: SzL a -> SzL a -> SzL a
append (SzL m xs) (SzL n ys) = SzL (m+n) (xs++ys)

joinsz :: SzL (SzL a) -> SzL a
joinsz = \ case
    SzL _ [] -> SzL 0 []
    SzL _ (sz:szs) -> append sz (foldr append nilsz szs)

instance Functor SzL where
    fmap :: (a -> b) -> SzL a -> SzL b
    fmap f (SzL m xs) = SzL m (f <$> xs)

instance Applicative SzL where
    pure :: a -> SzL a
    pure = singletonsz
    (<*>) :: SzL (a -> b) -> SzL a -> SzL b
    SzL m fs <*> SzL n xs = SzL (m*n) [f x | f <- fs, x <- xs]

instance Monad SzL where
    (>>=) :: SzL a -> (a -> SzL b) -> SzL b
    m >>= f = joinsz (f <$> m)
