module Main where

import Lib ( perms, magic3, magic4)
import Data.Time.Clock ( diffUTCTime, getCurrentTime )


main = do
   t0 <- getCurrentTime 
   print $ "start -- " ++ show t0 
   print $ length magic3
   t1 <- getCurrentTime 
   print $ diffUTCTime t1 t0