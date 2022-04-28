module Main where

import Lib ( perms, magics, magics4)
import Data.Time.Clock ( diffUTCTime, getCurrentTime )


main = do
   t0 <- getCurrentTime 
   print $ "start -- " ++ show t0 
   print $ length $ magics 4
   t1 <- getCurrentTime 
   print $ diffUTCTime t1 t0