module Tokenizer where

import Control.Applicative
import Control.Arrow
import Data.List
import Parser
import Token

eof' :: Parser String Token
eof' = Token EOF <$> ("" <$ eof)

tok2str :: Token -> String
tok2str (Token _ s') = s'

squish :: TokenType -> Parser [Token] [Token] -> Parser [Token] [Token]
squish ty = fmap (\toks -> [Token ty (concatMap tok2str toks)])

punctuation, digit :: Parser String Char
punctuation = oneOf ",.;:?!'\"-(){}[]" <?> "punctuation"
digit = oneOf "1234567890" <?> "a digit"

ws :: Parser String Token
ws = Token WS <$> kplus' (single (space <|> punctuation)) <?> "whitespace"

p, b, t, d, k, g, glottal, f, v, s, z, x, q, l, n, j, w, m, r, h :: Parser String Char
p = ichar 'p'
b = ichar 'b'
t = ichar 't'
d = ichar 'd'
k = ichar 'k'
g = ichar 'g'
glottal = char '\''
f = ichar 'f'
v = ichar 'v'
s = ichar 's'
z = ichar 'z'
x = ichar 'x'
q = ichar 'q'
l = ichar 'l'
n = ichar 'n'
j = ichar 'j'
w = ichar 'w'
m = ichar 'm'
r = ichar 'r'
h = ichar 'h'

a, e, i, o, u, y, aH, eH, iH, oH, uH, yH, aL, eL, iL, oL, uL, yL :: Parser String Char
a = ichar 'a'
e = ichar 'e'
i = ichar 'i'
o = ichar 'o'
u = ichar 'u'
y = ichar 'y'
aH = ichar 'á'
eH = ichar 'é'
iH = ichar 'í'
oH = ichar 'ó'
uH = ichar 'ú'
yH = ichar 'ý'
aL = ichar 'à'
eL = ichar 'è'
iL = ichar 'ì'
oL = ichar 'ò'
uL = ichar 'ù'
yL = ichar 'ỳ'

aN, eN, iN, oN, uN, yN, aHN, eHN, iHN, oHN, uHN, yHN :: Parser String Char
aN = ichar 'ä' <|> ichar 'ã'
eN = ichar 'ë' <|> ichar 'ẽ'
iN = ichar 'ï' <|> ichar 'ĩ'
oN = ichar 'ö' <|> ichar 'õ'
uN = ichar 'ü' <|> ichar 'ũ'
yN = ichar 'ÿ' <|> ichar 'ỹ'
aHN = ichar 'â'
eHN = ichar 'ê'
iHN = ichar 'î'
oHN = ichar 'ô'
uHN = ichar 'û'
yHN = ichar 'ŷ'

vow, vowH, vowL, vowN, vowHN, anyvow, anyH :: Parser String Char
vow = choice [a, e, i, o, u] <?> "unmarked vowel"
vowH = choice [aH, eH, iH, oH, uH] <?> "high vowel"
vowL = choice [aL, eL, iL, oL, uL] <?> "low vowel"
vowN = choice [aN, eN, iN, oN, uN] <?> "nasal vowel"
vowHN = choice [aHN, eHN, iHN, oHN, uHN] <?> "high nasal vowel"
anyvow = choice [vow, vowH, vowN, vowHN, y, yH, yHN, r] <?> "any vowel"
anyH = choice [vowH, vowHN, yH, yHN] <?> "any high vowel"

idipv, udipv, vdipg :: Parser String Char
idipv = choice [a, e, o, u] <?> "unmarked vowel except i"
udipv = choice [a, e, i, o] <?> "unmarked vowel except u"
vdipg = choice [a, e, o] <?> "unmarked vowel except i/u"

idipHv, udipHv, vdipHg :: Parser String Char
idipHv = choice [aH, aHN, eH, eHN, oH, oHN, uH, uHN] <?> "high vowel except i"
udipHv = choice [aH, aHN, eH, eHN, iH, iHN, oH, oHN] <?> "high vowel except u"
vdipHg = choice [aH, aHN, eH, eHN, oH, oHN] <?> "high vowel except i/u"

dip :: Parser String String
dip =
  inContext "unmarked diphthong" $
    choice
      [ i <.> idipv,
        u <.> udipv,
        vdipg <.> (i <|> u),
        r <.> (vow <|> y),
        (vow <|> y) <.> r <! (vow <|> y)
      ]

dipH :: Parser String String
dipH =
  inContext "high diphthong" $
    choice
      [ i <.> idipHv,
        u <.> udipHv,
        vdipHg <.> (i <|> u),
        r <.> choice [vowH, vowHN, yH, yHN],
        choice [vowH, vowHN, yH, yHN] <.> r <! anyvow
      ]

con, voiced, unvoiced, sibilant, fv, stop, sonorant, final, glide, fwc, fwf :: Parser String Char
con = choice [p, b, t, d, k, g, f, v, s, z, x, q, l, n] <?> "consonant (ptkbdg fsxvzq l n)"
voiced = choice [b, d, g, v, z, q] <?> "voiced obstruent (bdgvzq)"
unvoiced = choice [p, t, k, f, s, x] <?> "voiceless obstruent (ptkfsx)"
sibilant = choice [s, x, z, q] <?> "sibilant (szxq)"
fv = f <|> v <?> "nonsibilant (f or v)"
stop = choice [p, t, k, b, d, g] <?> "stop (ptkbdg)"
sonorant = l <|> n <?> "sonorant (l or n)"
final = choice [p, t, k, choice [b, d, g] <& voiced, l, n] <?> "root final"
glide = j <|> w <?> "glide (j or w)"
fwc = h <|> m <?> "freeword-only consonant (h or m)"
fwf = fv <|> sibilant <?> "fricative (fsxvqz; freeword final)"

anycon, anything :: Parser String Char
anycon = choice [con, fwc, glide, glottal, r] <?> "any consonant"
anything = choice [con, vow, fwc, vowH, vowN, vowHN, y, yH, yHN, glottal, glide] <?> "anything"

cl, badcl, fwcl, cl3 :: Parser String String
cl =
  inContext "CCV-root cluster" $
    choice
      [ (voiced &> sibilant) <.> (voiced &> stop <|> sonorant),
        (unvoiced &> sibilant) <.> (unvoiced &> stop <|> sonorant),
        ((t <|> d) !> stop <|> fv) <.> l,
        d <.> (voiced &> sibilant),
        t <.> (unvoiced &> sibilant)
      ]
badcl = choice [s <.> x, z <.> q, x <.> s, q <.> z]
fwcl =
  inContext "freeword cluster" $
    badcl
      !> choice [voiced <.> voiced, unvoiced <.> unvoiced, anychar <.> sonorant]
      &> (fwf <.> con)
cl3 =
  inContext "freeword triple" $
    final <.+> cl
      <|> (cl <|> fwcl) &> (con <.+> choice [cl, (anychar <.> fwf) !> fwcl])

root, rootN, rootH, rootL :: Parser String Token
root =
  fmap (Token Root) . inContext "root" $
    con <.> vow <! rootH <+.> final <! anyvow
      <|> cl <+.> vow
rootN =
  fmap (Token RootN) . inContext "nasal root" $
    con <.> vowHN <! rootH <+.> final <! anyvow
      <|> cl <+.> vowHN
rootH =
  fmap (Token RootH) . inContext "high root" $
    con <.> vowH <+.> final <! anyvow
      <|> cl <+.> vowH <! (cl <+.> vow <+.> final <++> (single con <|> tok2str <$> ws))
rootL =
  fmap (Token RootL) . inContext "low root" $
    con <.> vowL <+.> final <! anyvow
      <|> cl <+.> vowL

freewordStart, classicFreeword :: Parser String String
freeword :: Parser String Token
freewordStart =
  inContext "freeword start" $
    choice
      [ anycon
          <.> anyH
          <& (choice [final !> (cl3 <|> cl), fwcl, sibilant <.> m] <+.> anyvow),
        choice
          [ (single fwc <|> sibilant <.> glide) <++> (dipH <|> single anyH),
            option' (single glottal) <++> (dipH <|> single anyH),
            anycon <.+> dipH
          ]
          <& (choice [cl3, fwcl, cl, final <.> con, sibilant <.> m, single anycon] <+.> anyvow)
      ]
classicFreeword =
  inContext "classic freeword" $
    con <.> (vow <|> vowH) <++> fwcl <+.> vow <+.> final <++> kstar' (tok2str <$> root)

freeword =
  fmap (Token Freeword) . inContext "freeword" $
    classicFreeword
      <|> freewordStart
        <++> kstar'
          ( choice [cl3, fwcl, cl, final <.> con, single anycon]
              <++> choice [dipH, single anyH]
          )
        <++> choice [cl3, fwcl, cl, final <.> con, sibilant <.> m, single anycon]
        <++> choice [dip, single vow, single y]
        <++> option' (single final)

termhelper :: TokenType -> String -> Parser String Char -> Parser String Token
termhelper t' n' c' = fmap (Token t') ((root <|> freeword) !> ((c' <.> u) <! anyvow) <?> n')

ssterm, lsterm, dsterm, allterm, terminator :: Parser String Token
ssterm = termhelper SSTerm "gu" g
lsterm = termhelper LSTerm "ku" k
dsterm = termhelper DSTerm "vu" v
allterm = termhelper AllTerm "xu" x
terminator = inContext "terminator" $ ssterm <|> lsterm <|> dsterm

su :: Parser String Token
su = fmap (Token Su) . inContext "su" $ s <.> u

wlhelper :: [String] -> Parser String String
wlhelper = choice . map (foldr ((<.+>) . ichar) (pure ""))

justparticles :: Parser String ()
justparticles = dispeek (root <|> freeword) <?> "something NOT shaped like a verb"

suffix, possessive, numeral, quantifier, operator, quote, onomatopoeia :: Parser String Token
suffix =
  fmap (Token Suffix) . inContext "suffix" $
    wlhelper ["fu", "ko", "sa", "se", "si", "xo", "zi", "zu"] <! anyvow
possessive =
  fmap (Token Possessive) . inContext "possessive" $
    (l <|> n) <.> i <+.> a
      <|> choice [d, g, v, x] <.> u <+.> a
numeral =
  fmap (Token Numeral) . inContext "numeral" $
    choice
      [ wlhelper ["bui", "duo", "giu", "kua", "lio", "nue", "nu", "ne", "pei", "qau", "tie"],
        wlhelper ["xeu", "xai", "zou"],
        single digit
      ]
quantifier =
  fmap (Token Quantifier) . inContext "quantifier" $
    s <.+> choice [a <.> u, u <! u <.> vow, single u <! anyvow]
operator =
  fmap (Token Operator) . inContext "operator" $
    justparticles &> (wlhelper ["xi", "pu"] <! anyvow)
quote =
  fmap (Token Quote) . inContext "quote" $
    (l <.> (o <|> a) <+.> u)
      <++> kstar space
      <++> kplus (char '~')
      <++> kstar (noneOf "~")
      <++> kplus (char '~')
onomatopoeia =
  fmap (Token Onomatopoeia) . inContext "onomatopoeia" $
    option' (choice [cl3, cl, single anycon])
      <+.> y
      <++> kstar' (choice [cl3, fwcl, cl, final <.> con, single anycon] <+.> y)
      <++> option' (single final)

transVerb :: Parser String [Token]
transVerb =
  inContext "transmogrified verb" $
    (kplus (numeral <|> operator) <* optional ws <|> single possessive) <+.> suffix

morpheme :: Parser String Token
morpheme = root <|> suffix <* optional glottal

utility, utilityN :: Parser String Token
utility =
  fmap (Token Utility) . inContext "utility (bo/ka)" $
    justparticles &> (wlhelper ["bo", "ka"] <! anyvow)
utilityN =
  fmap (Token UtilityN) . inContext "nasal utility (bõ/kã)" $
    justparticles &> (wlhelper ["bõ", "kã"] <! anyvow)

compound, compoundH, compoundL, compoundN :: Parser String [Token]
compound = inContext "unmarked compound" $ root <.+> kplus morpheme
compoundH = inContext "high compound" $ rootH <.+> kplus morpheme
compoundL = inContext "low compound" $ rootL <.+> kplus morpheme
compoundN = inContext "nasal compound" $ rootN <.+> kplus morpheme

verb, verbH, verbL, verbN :: Parser String [Token]
verbH = inContext "high verb" $ compoundH <|> single rootH
verbL = inContext "low verb" $ compoundL <|> single rootL
verbN = inContext "nasal verb" $ compoundN <|> single rootN <|> single utilityN
verb =
  inContext "unmarked verb" $
    choice [compound, single root, single utility, single freeword, verbH, transVerb]

optional'orh :: Parser String String
optional'orh = option' (single h <|> single glottal)

transmogrifier :: Parser String Token
transmogrifier =
  fmap (Token Transmogrifier) . inContext "transmogrifier" $
    justparticles &> (optional'orh *> single u <! anyvow)

pronoun :: Parser String Token
pronoun =
  fmap (Token Pronoun) . inContext "pronoun" $
    justparticles
      &> choice
        [ wlhelper ["nie", "nio", "tua"],
          wlhelper ["ba", "bi", "gi", "go", "vi", "vo", "le", "li", "ni", "ti", "xe"] <! anyvow
        ]

casehelper :: TokenType -> String -> Parser String Char -> Parser String Token
casehelper t' n' v' =
  fmap (Token t') . inContext n' $
    justparticles
      &> choice
        [ t <.+> choice [u <.> v', v' <.> (i <|> u), single v' <! anyvow],
          optional'orh *> single v' <! anyvow
        ]

lsnom, lsacc, lsdat, lsprep, lsdet, lsbinder, lstag :: Parser String Token
lsnom = casehelper LSNom "long scope subject marker" oN
lsacc = casehelper LSAcc "long scope object marker" eN
lsdat = casehelper LSDat "long scope dative marker" aN
lsprep =
  fmap (Token LSPrep) . inContext "long scope preposition" $
    justparticles
      &> choice
        [ tok2str <$> choice [lsnom, lsacc, lsdat],
          wlhelper ["fãi", "fãu", "pãi", "pãu", "gẽi", "kiẽ", "kõu", "nãu", "nẽu", "pẽu", "piõ"],
          wlhelper ["vẽi", "või", "xãu", "xõi", "xuẽ"],
          wlhelper ["fẽ", "fĩ", "fõ", "zã"] <! anyvow
        ]
lsdet =
  fmap (Token LSDet) . inContext "long scope determiner" $
    justparticles
      &> choice
        [ wlhelper ["bãu", "kãu", "lãi"],
          wlhelper ["lã", "lõ", "lũ", "põ", "qĩ", "qũ", "tũ"] <! anyvow
        ]
lsbinder =
  fmap (Token LSBinder) . inContext "long scope binder" $
    justparticles
      &> choice
        [ wlhelper ["dõu", "dõi", "dẽu", "dẽi", "dãu", "dãi"],
          wlhelper ["dõ", "dẽ", "dã", "pĩ"] <! anyvow
        ]
lstag =
  fmap (Token LSTag) . inContext "long scope tag" $
    justparticles &> (wlhelper ["kĩ", "kẽ", "pẽ"] <! anyvow)

ssnom, ssacc, ssdat, ssprep, ssdet, ssbinder, sstag :: Parser String Token
ssnom = casehelper SSNom "short scope subject marker" o
ssacc = casehelper SSAcc "short scope object marker" e
ssdat = casehelper SSDat "short scope dative marker" a
ssprep =
  fmap (Token SSPrep) . inContext "short scope preposition" $
    justparticles
      &> choice
        [ tok2str <$> choice [ssnom, ssacc, ssdat],
          wlhelper ["fai", "fau", "pai", "pau", "gei", "kie", "kou", "nau", "neu", "peu", "pio"],
          wlhelper ["vei", "voi", "xau", "xoi", "xue"],
          wlhelper ["fe", "fi", "fo", "za"] <! anyvow
        ]
ssdet =
  fmap (Token SSDet) . inContext "short scope determiner" $
    justparticles
      &> choice
        [ wlhelper ["bau", "kau", "lai"],
          wlhelper ["la", "lo", "lu", "po", "qi", "qu", "tu"] <! anyvow,
          tok2str <$> possessive,
          kplus' (tok2str <$> numeral <|> tok2str <$> operator)
        ]
ssbinder =
  fmap (Token SSBinder) . inContext "short scope binder" $
    justparticles
      &> choice
        [ wlhelper ["dou", "doi", "deu", "dei", "dau", "dai"],
          wlhelper ["do", "de", "da", "pi"] <! anyvow
        ]
sstag =
  fmap (Token SSTag) . inContext "short scope tag" $
    justparticles &> (wlhelper ["ki", "ke", "pe"] <! anyvow)

preposition, determiner, tag, binder :: Parser String Token
preposition = inContext "preposition" $ lsprep <|> ssprep
determiner = inContext "determiner" $ lsdet <|> ssdet
tag = inContext "tag" $ lstag <|> sstag
binder = inContext "binder" $ lsbinder <|> ssbinder

discursiveIllocution, modalIllocution, illocution :: Parser String Token
discursiveIllocution =
  fmap (Token DiscursiveIllocution) . inContext "discursive illocution (i/je/ju)" $
    optional'orh *> single i <! anyvow <|> j <.> (e <|> u)
modalIllocution =
  fmap (Token ModalIllocution) . inContext "modal illocution (ja/jo/ji)" $ j <.> choice [a, o, i]
illocution =
  fmap (Token Illocution) . inContext "illocution" $
    choice
      [ tok2str <$> discursiveIllocution,
        tok2str <$> modalIllocution,
        wlhelper ["qai", "qei", "qoi", "zoi"]
      ]

modifier, verbModifier, connective, adverb, adverbSuffix :: Parser String Token
modifier =
  fmap (Token Modifier) . inContext "modifier" $
    justparticles
      &> choice [wlhelper ["fei", "sai", "sei", "soi"], wlhelper ["bu", "so", "no"] <! anyvow]
verbModifier =
  fmap (Token VerbModifier) . inContext "verb modifier" $
    justparticles &> (wlhelper ["ga", "ge"] <! anyvow)
connective =
  fmap (Token Connective) . inContext "connective" $
    justparticles &> choice [wlhelper ["poi"], wlhelper ["qa", "qe", "qo", "ze", "zo"] <! anyvow]
adverb =
  fmap (Token Adverb) . inContext "adverb" $
    justparticles
      &> choice
        [ wlhelper ["fia", "fie", "fio", "fiu", "fou", "kei", "nai", "noi", "qeu", "quo", "vai"],
          wlhelper ["vou", "wa", "we", "wi", "wo", "wu", "xou", "zei"],
          choice [n, x, f, p] <.> a <! anyvow
        ]
adverbSuffix =
  fmap (Token AdverbSuffix) . inContext "adverb suffix" $
    wlhelper ["niu", "lua", "die"]

adverbCompound :: Parser String [Token]
adverbCompound = inContext "adverb compound" $ adverb <.> adverbSuffix

--

anytoken :: Parser String Token
anytoken =
  choice
    [ choice [ws, root, rootN, rootH, rootL, freeword, ssterm, lsterm, dsterm],
      choice [allterm, suffix, possessive, numeral, quantifier, operator, onomatopoeia],
      choice [utility, utilityN, transmogrifier, pronoun, lsnom, lsacc, lsdat, lsprep, lsdet],
      choice [lsbinder, lstag, ssnom, ssacc, ssdat, ssprep, ssdet, ssbinder, sstag],
      choice [discursiveIllocution, modalIllocution, illocution, modifier, verbModifier],
      choice [connective, adverb, adverbSuffix, quote]
    ]

tokenize :: String -> Either ([ParseError String], Int) ([[Token]], Int)
tokenize = fmap (first nub) . parse (kstar (eof' !> anytoken) <* eof)
