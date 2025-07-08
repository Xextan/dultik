module Main where

import Parser
import Xextan

main :: IO ()
main = do
  putStrLn "doing the thing..."
  print $ parse (text <* eof) "xoi len"
