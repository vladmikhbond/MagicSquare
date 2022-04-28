{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

module Lib ( perms, magics, magics4) where

import Data.List ( (\\) )
import Control.Monad (when)
import Text.ParserCombinators.ReadP (many1)
--import Debug.Trace

type Square = [[Int]]

perms :: [Int] -> Int -> [[Int]]
perms xs 1 = map (:[]) xs
perms xs n = do
   x <- xs
   let xs' = xs \\ [x]
   rest <- perms xs' (n-1)
   return (x : rest)

-- for n = 4:   7040     1801 sec
magics :: Int -> [Square]
magics n = let
   ns = [1..n^2]
   nSum = (n^2 + 1) * n `div` 2
   level ns = do
      ys <- if length ns == n
         then perms ns n
         else filter ((nSum ==) . sum) (perms ns n)
      let ns' = ns \\ ys
      [(ys, ns')]

   stack [] = [[]]
   stack ns = do
        (lev, ns') <- level ns
        levs <- stack ns'
        [lev : levs]

   ok m = (and [ sum (map (!!i) m) == nSum | i <- [0..n-1]] ) &&
          sum [ m!!i!!i | i <- [0..n-1]] == nSum &&
          sum [ m!!i!!(n-i-1) | i <- [0..n-1]] == nSum
   in
    filter ok (stack ns)

-- for n = 4:   7040     1203 sec
magics4 :: [Square]
magics4 = let
   n = 4
   ns1 = [1..n^2]
   nSum = (n^2 + 1) * n `div` 2

   stack = do
      ys1 <- filter (\p -> nSum == sum p) (perms ns1 n)
      let ns2 = ns1 \\ ys1
      ys2 <- filter (\p -> nSum == sum p) (perms ns2 n)
      let ns3 = ns2 \\ ys2
      ys3 <- filter (\p -> nSum == sum p) (perms ns3 n)
      let ns4 = ns3 \\ ys3
      ys4 <- perms ns4 n

      return [ys1, ys2, ys3, ys4]

   ok m = sum (map (!!0) m) == nSum &&
          sum (map (!!1) m) == nSum &&
          sum (map (!!2) m) == nSum &&
          sum (map (!!3) m) == nSum &&
          m!!0!!0 + m!!1!!1 + m!!2!!2 + m!!3!!3 == nSum &&
          m!!0!!3 + m!!1!!2 + m!!2!!1 + m!!3!!0 == nSum
 in
    filter ok stack


   