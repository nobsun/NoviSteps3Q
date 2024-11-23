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
type O = Int

type Dom   = (I,[(I,I)])
type Codom = O

type Solver = Dom -> Codom

solve :: Solver
solve = \ case
    (w,wvs) -> knapsack w wvs

knapsack :: Int -> [(Int, Int)] -> Int
knapsack w wvs = iter [(0,0)] (sort wvs)
    where
        iter acc []
            = maximum (map snd acc)
        iter acc (wv:rs)
            = case dropWhile ((w <) . fst) (map (add wv) acc) of 
                [] -> maximum (map snd acc)
                acc' -> iter (acc' >< acc) rs
        add (x,y) (x',y') = (x+x',y+y')

(><) :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
[] >< ys = ys
xs >< [] = xs
xxs@(x@(wx,vx):xs) >< yys@(y@(wy,vy):ys) = case compare wx wy of
    LT -> case compare vx vy of
        LT -> y : (xxs >< ys)
        EQ -> xxs >< ys
        GT -> xxs >< ys
    EQ -> case compare vx vy of
        LT -> y : (xs >< ys)
        EQ -> x : (xs >< ys)
        GT -> x : (xs >< ys)
    GT -> case compare vx vy of
        LT -> xs >< yys 
        EQ -> xs >< yys
        GT -> x : (xs >< yys)

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    [_,w]:wvs -> case f (w,map toTuple wvs) of
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
    | n < 0  || r < 0  || n < r  = invalid
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

{- Union-Find -}
data UF
    = UF
    { parent :: IM.IntMap Int
    , size   :: IM.IntMap Int
    }

newUF :: Int -> Int -> UF
newUF s t
    = UF
    { parent = IM.fromList $ (,-1) <$> [s .. t]
    , size   = IM.fromList $ (,1)  <$> [s .. t]
    }

root :: UF -> Int -> Int
root uf = \ case
    x | p == -1   -> x
      | otherwise -> root uf p
      where
        p = uf.parent IM.! x

unite :: UF -> Int -> Int -> UF
unite uf x y = if
    | x' == y' -> uf
    | szx > szy -> update uf x' (y', szy)
    | otherwise -> update uf y' (x', szx)
    where
        x' = root uf x
        y' = root uf y
        szx = uf.size IM.! x'
        szy = uf.size IM.! y'
        update :: UF -> Int -> (Int, Int) -> UF
        update u a (b, szb)
            = u
            { parent = IM.insert b a u.parent
            , size   = IM.adjust (+ szb) a u.size
            }

isSame :: UF -> Int -> Int -> Bool
isSame uf x y = root uf x == root uf y
