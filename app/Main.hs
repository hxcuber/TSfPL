module Main where

import qualified CurryTypes (test)
import qualified LCNRTypes (test)
import qualified MLTypes (test)

main :: IO ()
main = do print CurryTypes.test
          print LCNRTypes.test
          print MLTypes.test
