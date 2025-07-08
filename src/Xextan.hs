module Xextan where

{-

basic description of what things do:

a "Parser a" is a parser that returns an 'a' for any type 'a'

<|> and choice[] are like peg "/" (except they try *all* of the options)

<.>  appends a char   to a char   (to create a string)
<.+> appends a string to a char
<+.> appends a char   to a string
<++> appends a string to a string

kplus', kstar', option' are like + * ? in peg

ichar matches a char case-insensitively

<?> and inContext label things. <?> is better for 'expected this' messages and inContext for 'while trying to do this' messages

a &> b is like "&a b" in peg
a <& b is like "a &b"

similar for !> and <!

-}

import Control.Applicative
import Data.Functor
import Parser

preprocess :: String -> String
preprocess = map (\c -> if c == '-' then ' ' else c) . filter (`notElem` "()«»‹›:")

-- character level --

punctuation :: Parser Char
punctuation = oneOf ",.;:?!'\"-(){}" <?> "punctuation"

digit :: Parser Char
digit = oneOf "1234567890" <?> "a digit"

ws :: Parser String
ws =
  kplus' (single (space <|> punctuation))
    <|> (eof $> "")
      <?> "whitespace"

quotationMark :: Parser String
quotationMark = kplus (char '~') <?> "quotation mark (tildes)"

glottal :: Parser Char
glottal = char '\'' <?> "glottal stop"

p, b, t, d, k, g, f, v, s, z, x, q, l, n, j, w, m, r, h :: Parser Char
p = ichar 'p' <?> "p"
b = ichar 'b' <?> "b"
t = ichar 't' <?> "t"
d = ichar 'd' <?> "d"
k = ichar 'k' <?> "k"
g = ichar 'g' <?> "g"
f = ichar 'f' <?> "f"
v = ichar 'v' <?> "v"
s = ichar 's' <?> "s"
z = ichar 'z' <?> "z"
x = ichar 'x' <?> "x"
q = ichar 'q' <?> "q"
l = ichar 'l' <?> "l"
n = ichar 'n' <?> "n"
j = ichar 'j' <?> "j"
w = ichar 'w' <?> "w"
m = ichar 'm' <?> "m"
r = ichar 'r' <?> "r"
h = ichar 'h' <?> "h"

a, e, i, o, u, y, aH, eH, iH, oH, uH, yH, aL, eL, iL, oL, uL, yL, aN, eN, iN, oN, uN, yN, aHN, eHN, iHN, oHN, uHN, yHN :: Parser Char
a = ichar 'a' <?> "a"
e = ichar 'e' <?> "e"
i = ichar 'i' <?> "i"
o = ichar 'o' <?> "o"
u = ichar 'u' <?> "u"
y = ichar 'y' <?> "y"
aH = ichar 'á' <?> "high a"
eH = ichar 'é' <?> "high e"
iH = ichar 'í' <?> "high i"
oH = ichar 'ó' <?> "high o"
uH = ichar 'ú' <?> "high u"
yH = ichar 'ý' <?> "high y"
aL = ichar 'à' <?> "low a"
eL = ichar 'è' <?> "low e"
iL = ichar 'ì' <?> "low i"
oL = ichar 'ò' <?> "low o"
uL = ichar 'ù' <?> "low u"
yL = ichar 'ỳ' <?> "low y"
aN = ichar 'ä' <|> ichar 'ã' <?> "nasal a"
eN = ichar 'ë' <|> ichar 'ẽ' <?> "nasal e"
iN = ichar 'ï' <|> ichar 'ĩ' <?> "nasal i"
oN = ichar 'ö' <|> ichar 'õ' <?> "nasal o"
uN = ichar 'ü' <|> ichar 'ũ' <?> "nasal u"
yN = ichar 'ÿ' <|> ichar 'ỹ' <?> "nasal y"
aHN = ichar 'â' <?> "high nasal a"
eHN = ichar 'ê' <?> "high nasal e"
iHN = ichar 'î' <?> "high nasal i"
oHN = ichar 'ô' <?> "high nasal o"
uHN = ichar 'û' <?> "high nasal u"
yHN = ichar 'ŷ' <?> "high nasal y"

vow, vowH, vowL, vowN, vowHN, anyvow, anyH :: Parser Char
vow = choice [a, e, i, o, u] <?> "unmarked vowel"
vowH = choice [aH, eH, iH, oH, uH] <?> "high vowel"
vowL = choice [aL, eL, iL, oL, uL] <?> "low vowel"
vowN = choice [aN, eN, iN, oN, uN] <?> "nasal vowel"
vowHN = choice [aHN, eHN, iHN, oHN, uHN] <?> "high nasal vowel"
anyvow = choice [vow, vowH, vowN, vowHN, y, yH, yHN, r] <?> "any vowel"
anyH = choice [vowH, vowHN, yH, yHN] <?> "any high vowel"

idipv, udipv, vdipg :: Parser Char
idipv = choice [a, e, o, u] <?> "unmarked vowel except i"
udipv = choice [a, e, i, o] <?> "unmarked vowel except u"
vdipg = choice [a, e, o] <?> "unmarked vowel except i/u"

idipHv, udipHv, vdipHg :: Parser Char
idipHv = choice [aH, aHN, eH, eHN, oH, oHN, uH, uHN] <?> "high vowel except i"
udipHv = choice [aH, aHN, eH, eHN, iH, iHN, oH, oHN] <?> "high vowel except u"
vdipHg = choice [aH, aHN, eH, eHN, oH, oHN] <?> "high vowel except i/u"

dip :: Parser String
dip =
  inContext "unmarked diphthong" $
    choice
      [ i <.> idipv,
        u <.> udipv,
        vdipg <.> (i <|> u),
        r <.> (vow <|> y),
        (vow <|> y) <.> r <! (vow <|> y)
      ]

dipH :: Parser String
dipH =
  inContext "high diphthong" $
    choice
      [ i <.> idipHv,
        u <.> udipHv,
        vdipHg <.> (i <|> u),
        r <.> choice [vowH, vowHN, yH, yHN],
        choice [vowH, vowHN, yH, yHN] <.> r <! anyvow
      ]

con, voiced, unvoiced, sibilant, fv, stop, sonorant, final, glide, fwc, fwf, anycon :: Parser Char
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
anycon = choice [con, fwc, glide, glottal, r] <?> "any consonant"

anything :: Parser Char
anything = choice [con, vow, fwc, vowH, vowN, vowHN, y, yH, yHN, glottal, glide] <?> "anything"

cl, badcl, fwcl, cl3 :: Parser String
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

-- word level --

root, rootN, rootH, rootL :: Parser String
root =
  inContext "root" $
    con <.> vow <! rootH <+.> final <! anyvow
      <|> cl <+.> vow
rootN =
  inContext "nasal root" $
    con <.> vowHN <! rootH <+.> final <! anyvow
      <|> cl <+.> vowHN
rootH =
  inContext "high root" $
    con <.> vowH <+.> final <! anyvow
      <|> cl <+.> vowH <! (cl <+.> vow <+.> final <++> (single con <|> ws)) -- eof is ws!
rootL =
  inContext "low root" $
    con <.> vowL <+.> final <! anyvow
      <|> cl <+.> vowL

freewordStart, classicFreeword, freeword :: Parser String
freewordStart =
  inContext "freeword start" $
    choice
      [ anycon
          <.> anyH
          <& ( choice [final !> (cl3 <|> cl), fwcl, sibilant <.> m]
                 <+.> anyvow
             ),
        choice
          [ (single fwc <|> sibilant <.> glide) <++> (dipH <|> single anyH),
            option' (single glottal) <++> (dipH <|> single anyH),
            anycon <.+> dipH
          ]
          <& ( choice [cl3, fwcl, cl, final <.> con, sibilant <.> m, single anycon]
                 <+.> anyvow
             )
      ]
classicFreeword =
  inContext "classic freeword" $
    con <.> (vow <|> vowH) <++> fwcl <+.> vow <+.> final <++> kstar' root
freeword =
  inContext "freeword" $
    classicFreeword
      <|> freewordStart
        <++> kstar'
          ( choice [cl3, fwcl, cl, final <.> con, single anycon]
              <++> choice [dipH, single anyH]
          )
        <++> choice [cl3, fwcl, cl, final <.> con, sibilant <.> m, single anycon]
        <++> choice [dip, single vow, single y]
        <++> option' (single final)

ssterm, lsterm, dsterm, allterm, terminator :: Parser String
ssterm = (root <|> freeword) !> (g <.> u <! anyvow) <?> "gu"
lsterm = (root <|> freeword) !> (k <.> u <! anyvow) <?> "ku"
dsterm = (root <|> freeword) !> (v <.> u <! anyvow) <?> "vu"
allterm = (root <|> freeword) !> (x <.> u <! anyvow) <?> "xu"
terminator = inContext "terminator" $ ssterm <|> lsterm <|> dsterm

suffix, possessive, numeral, quantifier, operator, transVerb, quoter, onomatopoeia :: Parser String
suffix =
  inContext "suffix" $
    choice
      [ x <.> o,
        k <.> o,
        z <.> i,
        s <.> a,
        s <.> e,
        s <.> i,
        f <.> u,
        z <.> u
      ]
      <! anyvow
possessive =
  inContext "possessive" $
    choice
      [ l <.> i <+.> a,
        n <.> i <+.> a,
        d <.> u <+.> a,
        g <.> u <+.> a,
        v <.> u <+.> a,
        x <.> u <+.> a
      ]
numeral =
  inContext "numeral" $
    choice
      [ z <.> o <+.> u,
        x <.> e <+.> u,
        q <.> a <+.> u,
        d <.> u <+.> o,
        t <.> i <+.> e,
        k <.> u <+.> a,
        p <.> e <+.> i,
        l <.> i <+.> o,
        x <.> a <+.> i,
        b <.> u <+.> i,
        g <.> i <+.> u,
        n <.> u <+.> e,
        n <.> u <! anyvow,
        n <.> e <! anyvow,
        single digit
      ]
quantifier =
  inContext "quantifier" $
    choice
      [ s <.> a <+.> u,
        s <.> u <! u <+.> vow,
        s <.> u <! anyvow
      ]
operator =
  inContext "operator (xi/pu)" $
    (root <|> freeword) !> ((x <.> i <|> p <.> u) <! anyvow)
transVerb =
  inContext "transmogrified verb" $
    ( kplus' (numeral <|> operator)
        <++> option' ws
        <|> possessive
    )
      <++> suffix
quoter =
  inContext "quoter (lou/lau)" $
    choice
      [ l <.> o <+.> u,
        l <.> a <+.> u
      ]
onomatopoeia =
  inContext "onomatopoeia" $
    option' (cl3 <|> cl <|> single anycon)
      <+.> y
      <++> kstar' (choice [cl3, fwcl, cl, final <.> con, single anycon] <+.> y)
      <++> option' (single final)

morpheme :: Parser String
morpheme = root <|> suffix <++> option' (single glottal)

utility, utilityN :: Parser String
utility =
  inContext "utility (bo/ka)" $ (root <|> freeword) !> ((b <.> o <|> k <.> a) <! anyvow)
utilityN =
  inContext "nasal utility (bõ/kã)" $ (root <|> freeword) !> ((b <.> oN <|> k <.> aN) <! anyvow)

compound, compoundH, compoundL, compoundN :: Parser String
compound = inContext "unmarked compound" $ root <++> kplus' morpheme
compoundH = inContext "high compound" $ rootH <++> kplus' morpheme
compoundL = inContext "low compound" $ rootL <++> kplus' morpheme
compoundN = inContext "nasal compound" $ rootN <++> kplus' morpheme

verb, verbH, verbL, verbN :: Parser String
verbH = inContext "high verb" $ compoundH <|> rootH
verbL = inContext "low verb" $ compoundL <|> rootL
verbN = inContext "nasal verb" $ compoundN <|> rootN <|> utilityN
verb = inContext "unmarked verb" $ choice [compound, root, utility, freeword, verbH, transVerb]

transmogrifier :: Parser String
transmogrifier =
  inContext "transmogrifier" $
    (root <|> freeword)
      !> (option' (single h <|> single glottal) <+.> u <! anyvow)

pronoun :: Parser String
pronoun =
  inContext "pronoun" $
    (root <|> freeword)
      !> choice
        [ n <.> i <+.> e,
          n <.> i <+.> o,
          t <.> u <+.> i,
          choice
            [ b <.> a,
              b <.> i,
              t <.> i,
              d <.> i,
              d <.> u,
              g <.> i,
              g <.> o,
              v <.> i,
              v <.> o,
              x <.> e,
              l <.> e,
              l <.> i,
              n <.> i
            ]
            <! anyvow
        ]

lsnom, lsacc, lsdat, lsprep, lsdet, lsbind, lstag :: Parser String
lsnom =
  inContext "long scope subject marker" $
    (root <|> freeword)
      !> choice
        [ t <.> u <+.> oN,
          t <.> oN <+.> i,
          t <.> oN <+.> u,
          option' (single h <|> single glottal) <+.> oN <+.> i,
          option' (single h <|> single glottal) <+.> oN <+.> u,
          t <.> oN <! anyvow,
          option' (single h <|> single glottal) <+.> oN <! anyvow
        ]
lsacc =
  inContext "long scope object marker" $
    (root <|> freeword)
      !> choice
        [ t <.> u <+.> eN,
          t <.> eN <+.> i,
          t <.> eN <+.> u,
          option' (single h <|> single glottal) <+.> eN <+.> i,
          option' (single h <|> single glottal) <+.> eN <+.> u,
          t <.> eN <! anyvow,
          option' (single h <|> single glottal) <+.> eN <! anyvow
        ]
lsdat =
  inContext "long scope dative marker" $
    (root <|> freeword)
      !> choice
        [ t <.> u <+.> aN,
          t <.> aN <+.> i,
          t <.> aN <+.> u,
          option' (single h <|> single glottal) <+.> aN <+.> i,
          option' (single h <|> single glottal) <+.> aN <+.> u,
          t <.> aN <! anyvow,
          option' (single h <|> single glottal) <+.> aN <! anyvow
        ]
lsprep =
  inContext "preposition" $
    (root <|> freeword)
      !> choice
        [ lsnom,
          lsacc,
          lsdat,
          p <.> i <+.> oN,
          k <.> i <+.> eN,
          x <.> u <+.> eN,
          p <.> aN <+.> i,
          f <.> aN <+.> i,
          v <.> eN <+.> i,
          v <.> oN <+.> i,
          x <.> oN <+.> i,
          p <.> aN <+.> u,
          p <.> eN <+.> u,
          k <.> oN <+.> u,
          f <.> aN <+.> u,
          x <.> aN <+.> u,
          n <.> aN <+.> u,
          n <.> eN <+.> u,
          g <.> eN <+.> i,
          f <.> oN <! anyvow,
          z <.> aN <! anyvow,
          f <.> eN <! anyvow,
          f <.> iN <! anyvow
        ]
lsdet =
  inContext "long scope determiner" $
    (root <|> freeword)
      !> choice
        [ b <.> aN <+.> u,
          k <.> aN <+.> u,
          l <.> aN <+.> i,
          choice
            [ q <.> iN,
              l <.> aN,
              l <.> oN,
              l <.> uN,
              t <.> uN,
              p <.> oN,
              q <.> uN
            ]
            <! anyvow
        ]
lsbind =
  inContext "long scope binder" $
    (root <|> freeword)
      !> choice
        [ d <.> oN <+.> u,
          d <.> oN <+.> i,
          d <.> eN <+.> u,
          d <.> eN <+.> i,
          d <.> aN <+.> u,
          d <.> aN <+.> i,
          d <.> oN <! anyvow,
          d <.> eN <! anyvow,
          d <.> aN <! anyvow,
          p <.> iN <! anyvow
        ]
lstag =
  inContext "long scope tag" $
    (root <|> freeword)
      !> ( choice
             [ k <.> iN,
               k <.> eN,
               p <.> eN
             ]
             <! anyvow
         )

ssnom, ssacc, ssdat, ssprep, ssdet, ssbind, sstag :: Parser String
ssnom =
  inContext "short scope subject marker" $
    (root <|> freeword)
      !> choice
        [ t <.> u <+.> o,
          t <.> o <+.> i,
          t <.> o <+.> u,
          option' (single h <|> single glottal) <+.> o <+.> i,
          option' (single h <|> single glottal) <+.> o <+.> u,
          t <.> o <! anyvow,
          option' (single h <|> single glottal) <+.> o <! anyvow
        ]
ssacc =
  inContext "short scope object marker" $
    (root <|> freeword)
      !> choice
        [ t <.> u <+.> e,
          t <.> e <+.> i,
          t <.> e <+.> u,
          option' (single h <|> single glottal) <+.> e <+.> i,
          option' (single h <|> single glottal) <+.> e <+.> u,
          t <.> e <! anyvow,
          option' (single h <|> single glottal) <+.> e <! anyvow
        ]
ssdat =
  inContext "short scope dative marker" $
    (root <|> freeword)
      !> choice
        [ t <.> u <+.> a,
          t <.> a <+.> i,
          t <.> a <+.> u,
          option' (single h <|> single glottal) <+.> a <+.> i,
          option' (single h <|> single glottal) <+.> a <+.> u,
          t <.> a <! anyvow,
          option' (single h <|> single glottal) <+.> a <! anyvow
        ]
ssprep =
  inContext "preposition" $
    (root <|> freeword)
      !> choice
        [ ssnom,
          ssacc,
          ssdat,
          p <.> i <+.> o,
          k <.> i <+.> e,
          x <.> u <+.> e,
          p <.> a <+.> i,
          f <.> a <+.> i,
          v <.> e <+.> i,
          v <.> o <+.> i,
          x <.> o <+.> i,
          p <.> a <+.> u,
          p <.> e <+.> u,
          k <.> o <+.> u,
          f <.> a <+.> u,
          x <.> a <+.> u,
          n <.> a <+.> u,
          n <.> e <+.> u,
          g <.> e <+.> i,
          f <.> o <! anyvow,
          z <.> a <! anyvow,
          f <.> e <! anyvow,
          f <.> i <! anyvow
        ]
ssdet =
  inContext "short scope determiner" $
    (root <|> freeword)
      !> choice
        [ b <.> a <+.> u,
          k <.> a <+.> u,
          l <.> a <+.> i,
          possessive,
          kplus' ((numeral <|> operator) <++> option' ws),
          choice
            [ q <.> i,
              l <.> a,
              l <.> o,
              l <.> u,
              t <.> u,
              p <.> o,
              q <.> u
            ]
            <! anyvow
        ]
ssbind =
  inContext "short scope binder" $
    (root <|> freeword)
      !> choice
        [ d <.> o <+.> u,
          d <.> o <+.> i,
          d <.> e <+.> u,
          d <.> e <+.> i,
          d <.> a <+.> u,
          d <.> a <+.> i,
          d <.> o <! anyvow,
          d <.> e <! anyvow,
          d <.> a <! anyvow,
          p <.> i <! anyvow
        ]
sstag =
  inContext "short scope tag" $
    (root <|> freeword)
      !> ( choice
             [ k <.> i,
               k <.> e,
               p <.> e
             ]
             <! anyvow
         )

preposition, determiner, tag, binder :: Parser String
preposition = inContext "preposition" $ lsprep <|> ssprep
determiner = inContext "determiner" $ lsdet <|> ssdet
tag = inContext "tag" $ lstag <|> sstag
binder = inContext "binder" $ lsbind <|> ssbind

discursiveIllocution, modalIllocution, illocution :: Parser String
discursiveIllocution =
  inContext "discursive illocution (i/je/ju)" $
    choice
      [ option' (single h <|> single glottal) <+.> i <! anyvow,
        j <.> e,
        j <.> u
      ]
modalIllocution =
  inContext "modal illocution (ja/jo/ji)" $
    choice
      [ j <.> a,
        j <.> o,
        j <.> i
      ]
illocution =
  inContext "illocution" $
    choice
      [ discursiveIllocution,
        modalIllocution,
        z <.> o <+.> i,
        q <.> a <+.> i,
        q <.> e <+.> i,
        q <.> o <+.> i
      ]

modifier, verbModifier, connective, adverb, adverbSuffix, adverbCompound :: Parser String
modifier =
  inContext "modifier" $
    (root <|> freeword)
      !> choice
        [ f <.> e <+.> i,
          s <.> a <+.> i,
          s <.> e <+.> i,
          s <.> o <+.> i,
          b <.> u <! anyvow,
          s <.> o <! anyvow,
          n <.> o <! anyvow
        ]
verbModifier =
  inContext "verb modifier" $
    (root <|> freeword)
      !> (g <.> (a <|> e) <! anyvow)
connective =
  inContext "connective" $
    (root <|> freeword)
      !> ( p <.> o <+.> i
             <|> choice
               [ q <.> a,
                 q <.> e,
                 q <.> o,
                 z <.> e,
                 z <.> o
               ]
               <! anyvow
         )
adverb =
  inContext "adverb" $
    (root <|> freeword)
      !> choice
        [ q <.> e <+.> u,
          v <.> o <+.> u,
          v <.> a <+.> i,
          n <.> o <+.> i,
          n <.> a <+.> i,
          z <.> e <+.> i,
          k <.> e <+.> i,
          q <.> u <+.> o,
          x <.> o <+.> u,
          f <.> o <+.> u,
          f <.> i <! i <+.> vow,
          w <.> vow,
          n <.> a <! anyvow,
          x <.> a <! anyvow,
          f <.> a <! anyvow,
          p <.> a <! anyvow
        ]
adverbSuffix =
  inContext "adverb suffix" $
    choice
      [ n <.> i <+.> u,
        l <.> u <+.> a,
        d <.> i <+.> e
      ]
adverbCompound = inContext "adverb compound" $ adverb <++> adverbSuffix

-- phrase level --

uElid, suElid, guElid, kuElid, vuElid, xu :: Parser String
uElid = option' transmogrifier
suElid = option' (s <.> u)
guElid = option' ssterm
kuElid = option' lsterm
vuElid = option' dsterm
xu = allterm

illocutions, modifiers, quotedText, quote :: Parser String
illocutions =
  inContext "illocutions" $
    choice
      [ illocution,
        discursiveIllocution <++> option' ws <++> modalIllocution,
        modalIllocution <++> option' ws <++> discursiveIllocution
      ]
modifiers = inContext "modifiers" $ kplus' (modifier <++> option' ws)
quotedText = inContext "quoted text" $ kplus' (quotationMark !> single anychar)
quote =
  inContext "quote" $
    option' modifiers
      <++> option' ws
      <++> quoter
      <++> option' ws
      <++> quotationMark
      <++> quotedText
      <++> quotationMark

adverbial, adverbPhrase, transAdverb, adverbs :: Parser String
adverbial = inContext "adverbial" $ adverbCompound <|> adverb <|> onomatopoeia
adverbPhrase =
  inContext "adverb phrase" $
    option' modifiers
      <++> adverbial
      <++> option' (option' ws <++> connective <++> option' ws <++> adverbPhrase)
transAdverb =
  inContext "transmogrified adverb" $
    option' ws
      <++> modifiers
      <& choice
        [ option' connective <++> option' ws <++> option' modifiers <++> illocution,
          ssterm,
          lsterm,
          dsterm,
          xu,
          eof $> ""
        ]
adverbs = inContext "adverbs" $ kplus' (adverbPhrase <++> option' ws) <|> transAdverb

freeNoun, freeConnectiveNoun :: Parser String
freeNoun =
  inContext "free noun" $
    suElid
      <++> option' modifiers
      <++> (pronoun <|> quote)
      <++> option' ws
      <++> kstar' tagPhrase
      <++> option' ws
      <++> guElid
      <++> option' (option' ws <++> connective <++> option' ws <++> freeNoun)
freeConnectiveNoun = inContext "connected free nouns" $ option' ws <++> connective <++> option' ws <++> freeNoun

termNucleus, predicateTerm, prepositionTerm, freeTerm, freeConnectiveTerm :: Parser String
termNucleus =
  inContext "term nucleus" $
    option' ws
      <++> kstar' ((prepositionTerm <|> adverbs) <++> option' ws)
      <++> choice
        [ freeTerm,
          choice
            [ predicate,
              tagPhrase,
              binderPhrase,
              option' modifiers <++> choice [pronoun, quote, verbL] <++> kstar' tagPhrase
            ]
            <++> option' ws
            <++> guElid
        ]
predicateTerm =
  inContext "predicate term" $
    choice
      [ option' ws
          <++> option' modifiers
          <++> transmogrifier
          <++> option' ws
          <++> (termNucleus <|> kstar' (choice [prepositionTerm, adverbs] <++> option' ws) <++> guElid),
        uElid
          <++> option' ws
          <++> choice [predicate, tagPhrase, binderPhrase]
          <++> option' ws
          <++> guElid
      ]
      <++> option' ws
      <++> option' (connective <++> option' ws <++> predicateTerm)
prepositionTerm =
  inContext "preposition term" $
    option' ws
      <++> option' modifiers
      <++> option' (verbModifier <++> option' ws)
      <++> choice
        [ ssprep
            <++> option' ws
            <++> (termNucleus <|> kstar' (choice [prepositionTerm, adverbs] <++> option' ws) <++> guElid),
          lsprep <++> option' ws <++> clause <++> option' ws <++> kuElid,
          ssprep <++> option' ws <& illocution <++> discourse
        ]
      <++> option' ws
      <++> option' (connective <++> option' ws <++> prepositionTerm)
freeTerm =
  inContext "free term" $
    option' ws
      <++> option' modifiers
      <++> choice
        [ kplus' (ssdet <++> option' ws)
            <++> (termNucleus <|> kstar' (choice [prepositionTerm, adverbs] <++> option' ws) <++> guElid),
          kstar' (ssdet <++> option' ws) <++> lsdet <++> option' ws <++> clause <++> option' ws <++> kuElid,
          ssdet <++> option' ws <& illocution <++> discourse
        ]
      <++> option' ws
      <++> option' (connective <++> option' ws <++> freeTerm)
freeConnectiveTerm =
  inContext "connected free terms" $
    option' ws <++> connective <++> option' ws <++> (prepositionTerm <|> freeTerm)

freeNominal, connectiveNominal, nounTerm, nounTerms, fragment :: Parser String
freeNominal =
  inContext "free nominal" $
    (freeTerm <|> freeNoun)
      <++> option' (option' ws <++> connective <++> freeNominal)
connectiveNominal =
  inContext "free nominal" $
    (freeConnectiveTerm <|> freeConnectiveNoun)
      <++> option' (option' ws <++> connective <++> freeNominal)
nounTerm = inContext "noun term" $ prepositionTerm <|> freeNominal <|> connectiveNominal
nounTerms = inContext "noun terms" $ kplus' (nounTerm <++> option' ws)
fragment = inContext "fragment" $ kplus' (nounTerms <|> adverbs)

verbal, serial, predicate :: Parser String
verbal =
  inContext "verbal" $
    option' (verbModifier <++> option' ws)
      <++> ( verb <++> option' ws <++> kstar' binderPhrase
               <|> verbN <++> option' ws <++> clause <++> option' ws <++> kuElid
           )
serial =
  inContext "serial" $
    option' modifiers
      <++> option' ws
      <++> (verbal <|> tagPhrase <|> binderPhrase)
      <++> kstar' (tagPhrase <|> binderPhrase)
      <++> kplus' (option' ws <++> (serial <|> option' modifiers <++> verbal))
predicate =
  inContext "predicate" $
    option' ws
      <++> option' modifiers
      <++> (serial <|> verbal)
      <++> kstar' tagPhrase
      <++> option' (option' ws <++> connective <++> option' ws <++> predicate)

frame, clause :: Parser String
frame = inContext "frame" $ predicateTerm <++> option' fragment
clause =
  inContext "clause" $
    kstar' (fragment <++> option' (xu <++> option' ws))
      <++> kplus' frame
      <++> option' (option' ws <++> connective <++> option' ws <++> clause)

binderPhrase, tagPhrase :: Parser String
binderPhrase =
  inContext "binder phrase" $
    option' ws
      <++> option' modifiers
      <++> choice
        [ ssbind
            <++> option' ws
            <++> option' adverbs
            <++> choice
              [ termNucleus,
                kstar' ((prepositionTerm <|> adverbs) <++> option' ws) <++> guElid
              ],
          lsbind <++> option' ws <++> clause <++> option' ws <++> kuElid
        ]
      <++> option' (option' ws <++> connective <++> option' ws <++> binderPhrase)
tagPhrase =
  inContext "tag phrase" $
    option' ws
      <++> option' modifiers
      <++> choice
        [ sstag
            <++> option' ws
            <++> option' adverbs
            <++> choice
              [ termNucleus,
                kstar' ((prepositionTerm <|> adverbs) <++> option' ws) <++> guElid
              ],
          lstag <++> option' ws <++> clause <++> option' ws <++> kuElid
        ]
      <++> option' (option' ws <++> connective <++> option' ws <++> tagPhrase)

-- text level --

sentence, discourse, text :: Parser String
sentence =
  inContext "sentence" $
    choice
      [ option' illocutions
          <++> option' ws
          <++> kplus' (choice [clause, fragment] <++> option' (xu <++> option' ws)),
        illocutions
          <++> option' ws
          <& choice [illocution, eof $> "", dsterm]
      ]
      <++> option'
        ( option' ws
            <++> connective
            <++> option' ws
            <& illocution
            <++> sentence
        )
discourse =
  inContext "discourse" $
    kplus' (option' ws <++> sentence)
      <++> option' ws
      <++> vuElid
      <++> option' (option' ws <++> connective <++> option' ws <++> discourse)
text = inContext "text" $ option' ws <++> kstar' (option' ws <++> discourse)
