module Token where

import Parser

data TokenType
  = Root
  | RootN
  | RootH
  | RootL
  | Freeword
  | SSTerm
  | LSTerm
  | DSTerm
  | AllTerm
  | Suffix
  | Possessive
  | Numeral
  | Quantifier
  | Operator
  | Quoter
  | Onomatopoeia
  | Utility
  | UtilityN
  | Transmogrifier
  | Pronoun
  | LSNom
  | LSAcc
  | LSDat
  | LSPrep
  | LSDet
  | LSBinder
  | LSTag
  | SSNom
  | SSAcc
  | SSDat
  | SSPrep
  | SSDet
  | SSBinder
  | SSTag
  | DiscursiveIllocution
  | ModalIllocution
  | Illocution
  | Modifier
  | VerbModifier
  | Connective
  | Adverb
  | AdverbSuffix
  | QuotationMark
  | Quote
  | Su
  | EOF
  | WS
  | TransVerb
  | VerbH
  | VerbL
  | VerbN
  | Verb
  deriving (Show, Eq)

data Token = Token TokenType String
  deriving (Eq)

instance Show Token where
  -- show :: Token -> String
  show (Token ty s) = show ty ++ ":" ++ s

token :: Token -> Parser [Token] Token
token = char

tt :: TokenType -> Parser [Token] Token
tt t = Parser $ \s pos ctx -> case s of
  [] -> [Failure (ParseError pos [show t] [Token EOF ""] ctx)]
  (tok@(Token t' _) : toks) ->
    if t' == t then [Success tok toks] else [Failure (ParseError pos [show t] [tok] ctx)]

singlet :: TokenType -> Parser [Token] [Token]
singlet = single . tt

optiont :: TokenType -> Parser [Token] [Token]
optiont = option' . singlet
