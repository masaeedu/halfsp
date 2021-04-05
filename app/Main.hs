module Main where

import Lib

main :: IO ()
main = do
  _ <- serverMain
  print "done!"
