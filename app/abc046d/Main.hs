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

type I = Char
type O = Int

type Dom   = [I]
type Codom = O

type Solver = Dom -> Codom

solve :: Solver
solve = \ case
    s -> fst $ foldl' phi (0,(0,0)) s where
        phi :: (Int,(Int,Int)) -> I -> (Int,(Int,Int))
        phi (c,(p,g)) = \ case
            'p' | p < g     -> (c,(succ p, g))
                | otherwise -> (pred c,(p, succ g))
            _   | p < g     -> (succ c,(succ p, g))
                | otherwise -> (c,(p, succ g))

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    s:_ -> case f s of
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

{- scanArray -}
scanArray :: (Ix i, Enum i)
          => (a -> b)
          -> (b -> a -> b)
          -> (b -> b -> b -> a -> b)
          -> Array (i,i) a -> Array (i,i) b
scanArray f g h sa = ta where
    ta  = listArray (bounds sa) (phi <$> assocs sa)
    phi = \ case
        (ij@(i,j),a)
            | ij == ij0 -> f a
            | i  == i0  -> g (ta ! second pred ij) a
            | j  == j0  -> g (ta ! first  pred ij) a
            | otherwise -> h (ta ! (pred *** pred) ij) (ta ! first  pred ij) (ta ! second pred ij) a
            where
                ij0 = fst (bounds sa)
                i0  = fst ij0
                j0  = snd ij0

{- neighborsArray -}
neighbors4 :: (Ix i, Enum i) => ((i,i),(i,i)) -> (i,i) -> [(i,i)]
neighbors4 (ij0@(i0,j0),hw@(h,w)) = \ case
    ij@(i,j) 
        | ij == ij0    -> [                second succ ij,                first succ ij]
        | ij == (i0,w) -> [second pred ij,                                first succ ij]
        | ij == hw     -> [second pred ij,                 first pred ij               ]
        | ij == (h,j0) -> [                second succ ij, first pred ij               ]
        | i  == i0     -> [second pred ij, second succ ij,                first succ ij]
        | j  == j0     -> [                second succ ij, first pred ij, first succ ij]
        | i  == h      -> [second pred ij, second succ ij, first pred ij               ]
        | j  == w      -> [second pred ij,                 first pred ij, first succ ij]
        | otherwise    -> [second pred ij, second succ ij, first pred ij, first succ ij]

neighbors4Array :: (Ix i, Enum i) => (a -> Bool, Array (i,i) a) -> Array (i,i) (S.Set (i,i))
neighbors4Array (p,a) = listArray (bounds a) (S.fromList . filter (p . (a !)) . neighbors4 (bounds a) <$> range (bounds a))

bfs4Array :: (Ix i, Enum i, Ord i)
          => Array (i,i) (S.Set (i,i)) -> (i,i) -> [S.Set (i,i)]
bfs4Array a ij = unfoldr psi (S.empty, S.singleton ij) where
    psi = \ case
        (vs,ns)
            | S.null ns' -> Nothing
            | otherwise  -> Just (ns, (vs',ns'))
            where
                ns' = S.difference (S.unions $ S.map (a !) ns) vs
                vs' = S.union vs ns

{- Cartesian Product -}
cp :: [[a]] -> [[a]]
cp = \ case
    []     -> [[]]
    xs:xss -> [ x : ys | x <- xs, ys <- yss ]
        where
            yss = cp xss

