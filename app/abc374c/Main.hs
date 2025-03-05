{-# LANGUAGE CPP #-}
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
import Control.Monad
import Data.Array
import Data.Bits
import Data.Bool
import Data.Char
import Data.Function
import Data.List
import Text.Printf

import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tree qualified as T
import Data.Vector qualified as V

import Debug.Trace qualified as Debug

debug :: Bool
debug = () == ()

type I = Int
type O = Int

type Dom   = [I]
type Codom = O

type Solver = Dom -> Codom

solve :: Solver
solve = \ case
    a:as -> uncurry max $ minimumBy (comparing $ abs . uncurry subtract) $ foldr phi [(a,0),(0,a)] as where
        phi x ys = concatMap (\ (y,z) -> [(x+y,z), (y,x+z)]) ys
    []   -> invalid $ show @Int __LINE__

toDom :: [[I]] -> Dom
toDom = \ case
    _:as:_ -> as
    _   -> invalid $ "toDom:" ++ show @Int __LINE__

fromCodom :: Codom -> [[O]]
fromCodom = \ case
    r -> [[r]]

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = fromCodom . f . toDom

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
impossible :: String -> a
impossible msg = error $ msg ++ ", impossible"

invalid :: String -> a
invalid msg = error $ msg ++ ", invalid input"

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
    _ -> error $ "combinations: " ++ show @Int __LINE__ ++ ", negative"

nCr :: Integral a => a -> a -> a
nCr n r
    | n < 0  || r < 0  || n < r  = error $ "nCr: " ++ show @Int __LINE__ ++ ", negative "
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
    _     -> error "toTuple: too short list"

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

{- modular arithmetic -}
base :: Int
base = 10^(9::Int) + 7

madd :: Int -> Int -> Int
madd !m !n = (m + n) `mod` base

msub :: Int -> Int -> Int
msub !m = madd m . negate

mmul :: Int -> Int -> Int
mmul !m !n = m * n `mod` base

mexpt :: Int -> Int -> Int
mexpt !b = \ case
    0             -> 1
    o | odd o     -> mmul b (mexpt b (pred o))
      | otherwise -> mexpt (mmul b b) (o `div` 2)

primesLT1000 :: [Int]
primesLT1000
    = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
      ,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199
      ,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293
      ,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397
      ,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499
      ,503,509,521,523,541,547,557,563,569,571,577,587,593,599
      ,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691
      ,701,709,719,727,733,739,743,751,757,761,769,773,787,797
      ,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887
      ,907,911,919,929,937,941,947,953,967,971,977,983,991,997]
