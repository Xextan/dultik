{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Data.Ord

data ParseError = ParseError
  { position :: Int,
    expected :: [String],
    found :: String,
    context :: [String]
  }
  deriving (Show, Eq)

data ParseResult a = Success a String | Failure ParseError
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Int -> [String] -> [ParseResult a]}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s pos ctx ->
    map
      ( \case
          Success x rest -> Success (f x) rest
          Failure e -> Failure e
      )
      (p s pos ctx)

instance Applicative Parser where
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

instance Alternative Parser where
  empty = Parser $ \_ _ _ -> []
  Parser p1 <|> Parser p2 = Parser $ \s pos ctx -> lazyconcat (p1 s pos ctx) (p2 s pos ctx)

lazyconcat :: [a] -> [a] -> [a]
lazyconcat [] ys = ys
lazyconcat xs [] = xs
lazyconcat (x : xs) ys = x : lazyconcat xs ys

instance Monad Parser where
  Parser p >>= f = Parser $ \s pos ctx ->
    concatMap
      ( \case
          Success x rest -> runParser (f x) rest (pos + length s - length rest) ctx
          Failure err -> [Failure err]
      )
      (p s pos ctx)

infixl 0 <?>

(<?>) :: Parser a -> String -> Parser a
Parser p <?> msg = Parser $ \s pos ctx ->
  case p s pos ctx of
    [] -> [Failure $ ParseError pos [msg] (take 10 s) ctx]
    results -> map (addExpected msg) results
  where
    addExpected _ (Success x rest) = Success x rest
    addExpected msg' (Failure err) = Failure err {expected = msg' : expected err}

inContext :: String -> Parser a -> Parser a
inContext ctx (Parser p) = Parser $ \s pos ctxs ->
  p s pos (ctx : ctxs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s pos ctx -> case s of
  (c : cs) | f c -> [Success c cs]
  _ -> [Failure $ ParseError pos [] (take 1 s) ctx]

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

noneOf :: String -> Parser Char
noneOf cs = satisfy (`notElem` cs)

char :: Char -> Parser Char
char c = satisfy (== c) <?> show c

anychar :: Parser Char
anychar = satisfy (const True)

string :: String -> Parser String
string str = Parser $ \s pos ctx ->
  if str `isPrefixOf` s
    then [Success str (drop (length str) s)]
    else [Failure $ ParseError pos [str] (take (length str) s) ctx]

ichar :: Char -> Parser Char
ichar c = satisfy (\x -> toLower x == toLower c)

istring :: String -> Parser String
istring str = Parser $ \s pos ctx ->
  if map toLower str `isPrefixOf` map toLower s
    then [Success (take (length str) s) (drop (length str) s)]
    else [Failure $ ParseError pos [str] (take (length str) s) ctx]

-- kstar/kplus are named after */+ in regex because i like those
kstar, many0, kplus, many1 :: Parser a -> Parser [a]
kstar = many
many0 = many
kplus = some
many1 = some

kstar', many0', kplus', many1' :: Parser [a] -> Parser [a]
kstar' p = concat <$> many p
many0' p = concat <$> many p
kplus' p = concat <$> some p
many1' p = concat <$> some p

count :: Int -> Parser a -> Parser [a]
count = replicateM

atLeast :: Int -> Parser a -> Parser [a]
atLeast n p = (++) <$> count n p <*> kstar p

atMost :: Int -> Parser a -> Parser [a]
atMost 0 _ = pure []
atMost n p = (:) <$> p <*> atMost (n - 1) p <|> pure []

range :: Int -> Int -> Parser a -> Parser [a]
range m n p = (++) <$> count m p <*> atMost (n - m) p

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = kstar space

spaces1 :: Parser String
spaces1 = kplus space

-- negative lookahead
dispeek :: Parser a -> Parser ()
dispeek (Parser p) = Parser $ \s pos ctx ->
  case p s pos ctx of
    results | any isSuccess results -> [Failure $ ParseError pos [] s ctx]
    _ -> [Success () s]
  where
    isSuccess (Success _ _) = True
    isSuccess _ = False

-- positive lookahead
peek :: Parser a -> Parser a
peek (Parser p) = Parser $ \s pos ctx -> case p s pos ctx of
  results | any isSuccess results -> [Success x s | Success x _ <- results]
  failures -> failures
  where
    isSuccess (Success _ _) = True
    isSuccess _ = False

infixr 4 &>, !>

infixl 4 <&, <!

(&>), (!>) :: Parser a -> Parser b -> Parser b
g &> p = peek g *> p
g !> p = dispeek g *> p

(<&), (<!) :: Parser a -> Parser b -> Parser a
p <& g = p <* peek g
p <! g = p <* dispeek g

skip :: Parser a -> Parser ()
skip = void

eof :: Parser ()
eof = Parser $ \s pos ctx -> case s of
  "" -> [Success () ""]
  _ -> [Failure $ ParseError pos ["end of input"] (take 5 s) ctx]

choice :: [Parser a] -> Parser a
choice = asum

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

option :: a -> Parser a -> Parser a
option def p = p <|> pure def

option' :: Parser [a] -> Parser [a]
option' = option []

single :: Parser a -> Parser [a]
single p = (: []) <$> p

eol :: Parser String
eol = string "\n" <|> string "\r\n" <|> string "\r" <?> "end of line"

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (op <*> pure x <*> p >>= rest) <|> pure x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (op <*> pure x <*> chainr1 p op) <|> pure x

sepBy :: Parser a -> Parser sep -> Parser [a]
p `sepBy` sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p)

-- fixed point recursion
fix :: (Parser a -> Parser a) -> Parser a
fix f = let p = f p in p

parse :: Parser a -> String -> Either ParseError ([a], Int)
parse p s = case runParser p s 0 [] of
  [] -> Left $ ParseError 0 ["anything"] s [] -- shouldn't ever happen
  results ->
    let successes = [x | Success x _ <- results]
        failures = [e | Failure e <- results]
     in case successes of
          [] -> case failures of
            [] -> Left $ ParseError (length s) ["end of input"] "" []
            fs -> Left $ maximumBy (comparing position) fs
          xs -> Right (xs, length xs)

-- other useful operators

infixl 4 <.>, <++>, <.+>, <+.>

(<.>) :: Parser a -> Parser a -> Parser [a]
p1 <.> p2 = liftA2 (\x y -> [x, y]) p1 p2

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
p1 <++> p2 = (++) <$> p1 <*> p2

(<.+>) :: Parser a -> Parser [a] -> Parser [a]
p1 <.+> p2 = (:) <$> p1 <*> p2

(<+.>) :: Parser [a] -> Parser a -> Parser [a]
p1 <+.> p2 = (\xs x -> xs ++ [x]) <$> p1 <*> p2
