module Main where

import GHC.OldList hiding (concatMap, length, null)
import Parser
import Token
import Tokenizer hiding (s)
import Xextan

main :: IO ()
main = test "jo tak xextan"

gray, red, green, yellow, blue, pink, cyan :: String -> String
gray s = "\x1b[90m" ++ s ++ "\x1b[m"
red s = "\x1b[91m" ++ s ++ "\x1b[m"
green s = "\x1b[92m" ++ s ++ "\x1b[m"
yellow s = "\x1b[93m" ++ s ++ "\x1b[m"
blue s = "\x1b[94m" ++ s ++ "\x1b[m"
pink s = "\x1b[95m" ++ s ++ "\x1b[m"
cyan s = "\x1b[96m" ++ s ++ "\x1b[m"

-- for ghci convenience
test :: String -> IO ()
test input =
  case tokenize input of
    Left (errs, c) -> do
      mapM_ (putStrLn . gray . show) errs
      putStrLn . red $ "couldn't tokenize, " ++ show c ++ " error paths"
    Right (tokenlists, _) ->
      let dedup = nub tokenlists
          nonws = map (filter (\(Token ty _) -> ty /= WS)) dedup
          results = map (parse (text <* eof)) nonws
          successes = [(s, c) | Right (s, c) <- results]
       in if null successes
            then do
              mapM_ (putStrLn . yellow . show) nonws
              putStrLn . red $ "tokenized fine (" ++ show (length dedup) ++ " unique tokenizations) but none of them parse"
            else
              mapM_ (putStrLn . green . show) . nub $ concatMap fst successes
