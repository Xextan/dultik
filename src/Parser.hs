{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

{-
type names:
i [j] - input
a [b] - output
-}

data ParseError i = ParseError
  { position :: Int,
    expected :: [String],
    found :: i,
    context :: [String]
  }
  deriving (Show, Eq)

data ParseResult i a = Success a i | Failure (ParseError i)
  deriving (Show, Eq)

newtype Parser i a = Parser {runParser :: i -> Int -> [String] -> [ParseResult i a]}

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \s pos ctx ->
    map
      ( \case
          Success x rest -> Success (f x) rest
          Failure e -> Failure e
      )
      (p s pos ctx)

instance Applicative (Parser [j]) where
  pure x = Parser $ \s _ _ -> [Success x s]
  Parser pf <*> Parser px = Parser $ \s pos ctx ->
    concatMap
      ( \case
          Failure err -> [Failure err]
          Success f rest ->
            let pos' = pos + length s - length rest
             in map
                  ( \case
                      Failure err -> Failure err
                      Success x rest' -> Success (f x) rest'
                  )
                  (px rest pos' ctx)
      )
      (pf s pos ctx)

instance Alternative (Parser [j]) where
  empty = Parser $ \_ _ _ -> []
  Parser p1 <|> Parser p2 = Parser $ \s pos ctx -> lazyconcat (p1 s pos ctx) (p2 s pos ctx)

lazyconcat :: [x] -> [x] -> [x]
lazyconcat [] ys = ys
lazyconcat xs [] = xs
lazyconcat (x : xs) ys = x : lazyconcat xs ys

instance Monad (Parser [j]) where
  Parser p >>= f = Parser $ \s pos ctx ->
    concatMap
      ( \case
          Success x rest -> runParser (f x) rest (pos + length s - length rest) ctx
          Failure err -> [Failure err]
      )
      (p s pos ctx)

infixl 0 <?>

(<?>) :: Parser [j] a -> String -> Parser [j] a
Parser p <?> msg = Parser $ \s pos ctx ->
  case p s pos ctx of
    [] -> [Failure $ ParseError pos [msg] (take 10 s) ctx]
    results -> map (addExpected msg) results
  where
    addExpected _ (Success x rest) = Success x rest
    addExpected msg' (Failure err) = Failure err {expected = msg' : expected err}

inContext :: String -> Parser i a -> Parser i a
inContext ctx (Parser p) = Parser $ \s pos ctxs ->
  p s pos (ctx : ctxs)

satisfy :: (j -> Bool) -> Parser [j] j
satisfy f = Parser $ \s pos ctx -> case s of
  (c : cs) | f c -> [Success c cs]
  _ -> [Failure $ ParseError pos [] (take 1 s) ctx]

oneOf :: (Eq j) => [j] -> Parser [j] j
oneOf cs = satisfy (`elem` cs)

noneOf :: (Eq j) => [j] -> Parser [j] j
noneOf cs = satisfy (`notElem` cs)

char :: (Eq j, Show j) => j -> Parser [j] j
char c = satisfy (== c) <?> show c

anychar :: Parser [j] j
anychar = satisfy (const True)

string :: (Eq j, Show j) => [j] -> Parser [j] [j]
string str = Parser $ \s pos ctx ->
  if str `isPrefixOf` s
    then [Success str (drop (length str) s)]
    else [Failure $ ParseError pos [show str] (take (length str) s) ctx]

ichar :: Char -> Parser String Char
ichar c = satisfy (\x -> toLower x == toLower c)

istring :: String -> Parser String String
istring str = Parser $ \s pos ctx ->
  if map toLower str `isPrefixOf` map toLower s
    then [Success (take (length str) s) (drop (length str) s)]
    else [Failure $ ParseError pos [str] (take (length str) s) ctx]

-- kstar/kplus are named after */+ in regex because i like those
kstar, many0, kplus, many1 :: Parser [j] b -> Parser [j] [b]
kstar = many
many0 = many
kplus = some
many1 = some

kstar', many0', kplus', many1' :: Parser [j] [b] -> Parser [j] [b]
kstar' p = concat <$> many p
many0' p = concat <$> many p
kplus' p = concat <$> some p
many1' p = concat <$> some p

count :: Int -> Parser [j] b -> Parser [j] [b]
count = replicateM

atLeast :: Int -> Parser [j] b -> Parser [j] [b]
atLeast n p = (++) <$> count n p <*> kstar p

atMost :: Int -> Parser [j] b -> Parser [j] [b]
atMost 0 _ = pure []
atMost n p = (:) <$> p <*> atMost (n - 1) p <|> pure []

range :: Int -> Int -> Parser [j] b -> Parser [j] [b]
range m n p = (++) <$> count m p <*> atMost (n - m) p

space :: Parser String Char
space = satisfy isSpace

spaces :: Parser String String
spaces = kstar space

spaces1 :: Parser String String
spaces1 = kplus space

-- negative lookahead
dispeek :: Parser i a -> Parser i ()
dispeek (Parser p) = Parser $ \s pos ctx ->
  case p s pos ctx of
    results | any isSuccess results -> [Failure $ ParseError pos [] s ctx]
    _ -> [Success () s]
  where
    isSuccess (Success _ _) = True
    isSuccess _ = False

-- positive lookahead
peek :: Parser i a -> Parser i a
peek (Parser p) = Parser $ \s pos ctx -> case p s pos ctx of
  results | any isSuccess results -> [Success x s | Success x _ <- results]
  failures -> failures
  where
    isSuccess (Success _ _) = True
    isSuccess _ = False

infixr 4 &>, !>

infixl 4 <&, <!

(&>), (!>) :: Parser [j] a -> Parser [j] x -> Parser [j] x
g &> p = peek g *> p
g !> p = dispeek g *> p

(<&), (<!) :: Parser [j] a -> Parser [j] x -> Parser [j] a
p <& g = p <* peek g
p <! g = p <* dispeek g

skip :: Parser i a -> Parser i ()
skip = void

eof :: Parser [j] ()
eof = Parser $ \s pos ctx -> case s of
  [] -> [Success () []]
  _ -> [Failure $ ParseError pos ["end of input"] (take 5 s) ctx]

choice :: [Parser [j] a] -> Parser [j] a
choice = asum

between :: Parser [j] open -> Parser [j] close -> Parser [j] a -> Parser [j] a
between open close p = open *> p <* close

option :: a -> Parser [j] a -> Parser [j] a
option def p = p <|> pure def

option' :: Parser [j] [b] -> Parser [j] [b]
option' = option []

single :: Parser i b -> Parser i [b]
single p = (: []) <$> p

eol :: Parser String String
eol = string "\n" <|> string "\r\n" <|> string "\r" <?> "end of line"

chainl1 :: Parser [j] a -> Parser [j] (a -> a -> a) -> Parser [j] a
chainl1 p op = p >>= rest
  where
    rest x = (op <*> pure x <*> p >>= rest) <|> pure x

chainr1 :: Parser [j] a -> Parser [j] (a -> a -> a) -> Parser [j] a
chainr1 p op = p >>= rest
  where
    rest x = (op <*> pure x <*> chainr1 p op) <|> pure x

sepBy :: Parser [j] b -> Parser [j] sep -> Parser [j] [b]
p `sepBy` sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser [j] b -> Parser [j] sep -> Parser [j] [b]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p)

-- fixed point recursion
fix :: (Parser i a -> Parser i a) -> Parser i a
fix f = let p = f p in p

parse :: (Eq j) => Parser [j] b -> [j] -> Either ([ParseError [j]], Int) ([b], Int)
parse p s = case runParser p s 0 [] of
  [] -> Left ([ParseError 0 ["anything"] s []], 1) -- shouldn't ever happen
  results ->
    let successes = [x | Success x _ <- results]
        failures = [e | Failure e <- results]
     in case successes of
          [] -> case failures of
            [] -> Left ([ParseError (length s) ["end of input"] [] []], 1)
            fs ->
              let maxp = maximum (map position fs)
                  fs' = nub [e | e <- fs, position e == maxp]
               in Left (fs', length fs')
          xs -> Right (xs, length xs)

-- other useful operators

infixl 4 <.>, <++>, <.+>, <+.>

(<.>) :: Parser [j] b -> Parser [j] b -> Parser [j] [b]
p1 <.> p2 = liftA2 (\x y -> [x, y]) p1 p2

(<++>) :: Parser [j] [b] -> Parser [j] [b] -> Parser [j] [b]
p1 <++> p2 = (++) <$> p1 <*> p2

(<.+>) :: Parser [j] b -> Parser [j] [b] -> Parser [j] [b]
p1 <.+> p2 = (:) <$> p1 <*> p2

(<+.>) :: Parser [j] [b] -> Parser [j] b -> Parser [j] [b]
p1 <+.> p2 = (\xs x -> xs ++ [x]) <$> p1 <*> p2
