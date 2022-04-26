{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

module Lib ( perm, magic3, magic4, someFunc) where

import Data.List ( (\\) )
import Control.Monad (when)

someFunc :: IO ()
someFunc = putStrLn "someFunc"



perm xs 1 = map (:[]) xs
perm xs n = do 
   x <- xs
   let xs' = xs \\ [x]
   rest <- perm xs' (n-1)
   return (x : rest)

magic3 = let 
   n = 3
   xs = [1..n^2]
   nSum = (n^2 + 1) * n `div` 2

   stack = do
      ys <- filter (\p -> nSum == sum p) (perm xs n)
      let xs1 = xs \\ ys
      ys1 <- filter (\p -> nSum == sum p) (perm xs1 n)
      let xs2 = xs1 \\ ys1
      ys2 <- filter (\p -> nSum == sum p) (perm xs2 n)
      return [ys, ys1, ys2]

   ok m = sum (map (!!0) m) == nSum && 
          sum (map (!!1) m) == nSum && 
          sum (map (!!2) m) == nSum &&
          m!!0!!0 + m!!1!!1 + m!!2!!2 == nSum && 
          m!!0!!2 + m!!1!!1 + m!!2!!0 == nSum                
 in 
    filter ok stack


magic4 = let 
   n = 4
   xs = [1..n^2]
   nSum = (n^2 + 1) * n `div` 2

   stack = do
      ys <- filter (\p -> nSum == sum p) (perm xs n)
      let xs1 = xs \\ ys
      ys1 <- filter (\p -> nSum == sum p) (perm xs1 n)
      let xs2 = xs1 \\ ys1
      ys2 <- filter (\p -> nSum == sum p) (perm xs2 n)
      let xs3 = xs2 \\ ys2
      ys3 <- filter (\p -> nSum == sum p) (perm xs3 n)
      return [ys, ys1, ys2, ys3]

   ok m = sum (map (!!0) m) == nSum && 
          sum (map (!!1) m) == nSum && 
          sum (map (!!2) m) == nSum &&
          sum (map (!!3) m) == nSum &&
          m!!0!!0 + m!!1!!1 + m!!2!!2 + m!!3!!3 == nSum && 
          m!!0!!3 + m!!1!!2 + m!!2!!1 + m!!3!!0 == nSum                
 in 
    filter ok stack


   