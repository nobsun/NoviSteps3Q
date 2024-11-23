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

type I = Int
type O = String

type Solver = (V2d I, V2d I, V2d I, V2d I) -> O

solve :: Solver
solve = \ case
    (a,b,c,d) -> bool "No" "Yes" $ segmentsIntersect a b c d

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    xys -> case toTuple <$> xys of
        [a,b,c,d] -> case f (a,b,c,d) of
            r         -> [[r]]
        _         -> error "wrap: invalid input format"

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

instance InterfaceForOJS Int where
    readB :: B.ByteString -> Int
    readB = readInt
    showB :: Int -> B.ByteString
    showB = showInt

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

readStr :: B.ByteString -> String
readStr = B.unpack

showStr :: String -> B.ByteString
showStr = B.pack

readDbl :: B.ByteString -> Double
readDbl = read . B.unpack

showDbl :: Double -> B.ByteString
showDbl = B.pack . show

{- Bonsai -}

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
>>> subsegments "yay"
[["y","a","y"],["ya","ay"],["yay"]]
-}
subsegments :: [a] -> [[[a]]]
subsegments = tail . transpose . map inits . transpose . tails 

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

{- error -}
impossible :: a
impossible = error "impossible"

invalid :: a
invalid = error "invalid input"

{- debug -}
trace :: String -> a -> a
trace | debug     = Debug.trace
      | otherwise = const id

tracing :: Show a => a -> a
tracing = trace . show <*> id

{- 2D -}
type V2d a = (a,a)
add2d :: (Num a) => V2d a -> V2d a -> V2d a
add2d (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
sub2d :: (Num a) => V2d a -> V2d a -> V2d a
sub2d = add2d . neg2d
neg2d :: (Num a) => V2d a -> V2d a
neg2d = negate *** negate

cprod2d :: (Num a) => V2d a -> V2d a -> a
cprod2d (x1,y1) (x2,y2) = x1 * y2 - x2 * y1

{- | 線分の交差判定
>>> p1 = (0,0) :: V2d Int
>>> p2 = (2,4) :: V2d Int
>>> p3 = (4,8) :: V2d Int
>>> p4 = (0,5) :: V2d Int
>>> p5 = (4,3) :: V2d Int
>>> p6 = (6,2) :: V2d Int
>>> segmentsIntersect p1 p3 p4 p6
True
>>> segmentsIntersect p1 p2 p4 p6
True
>>> segmentsIntersect p1 p3 p5 p6
False
>>> segmentsIntersect p4 p2 p5 p6
False
-}
segmentsIntersect :: (Num a, Ord a) => V2d a -> V2d a -> V2d a -> V2d a -> Bool
segmentsIntersect p1 p2 p3 p4
    = or
    [ d1 * d2 < 0 && d3 * d4 < 0
    , d1 == 0 && onSegment2d p3 p4 p1
    , d2 == 0 && onSegment2d p3 p4 p2
    , d3 == 0 && onSegment2d p1 p2 p3
    , d4 == 0 && onSegment2d p1 p2 p4
    ]
    where
        d1 = direction2d p3 p4 p1
        d2 = direction2d p3 p4 p2
        d3 = direction2d p1 p2 p3
        d4 = direction2d p1 p2 p4

direction2d :: (Num a) => V2d a -> V2d a -> V2d a -> a
direction2d p1 p2 p3
    = signum $ sub2d p1 p3 `cprod2d` sub2d p1 p2

onSegment2d :: (Num a, Ord a) => V2d a -> V2d a -> V2d a -> Bool
onSegment2d (x1,y1) (x2,y2) (x3,y3)
    = and
    [ min x1 x2 <= x3
    , x3 <= max x1 x2
    , min y1 y2 <= y3
    , y3 <= max y1 y2
    ]
