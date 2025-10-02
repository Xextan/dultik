module Xextan where

import Control.Applicative
import Parser
import Token
import Tokenizer (squish)

preprocess :: String -> String
preprocess = map (\c -> if c == '-' then ' ' else c) . filter (`notElem` "()«»‹›:")

transVerbT :: Parser [Token] [Token]
transVerbT =
  squish TransVerb $
    (kplus' (singlet Numeral <|> singlet Operator) <|> singlet Possessive)
      <++> singlet Suffix

verbLT, verbHT, verbNT, verbT :: Parser [Token] [Token]
verbLT = squish VerbL $ singlet RootL <|> (singlet RootL <++> kplus' (singlet Root <|> singlet Suffix))
verbHT = squish VerbH $ singlet RootH <|> (singlet RootH <++> kplus' (singlet Root <|> singlet Suffix))
verbNT =
  squish VerbN $
    singlet RootN
      <|> singlet UtilityN
      <|> (singlet RootN <++> kplus' (singlet Root <|> singlet Suffix))
verbT = squish Verb $ singlet Root <|> singlet Utility <|> singlet Freeword <|> verbHT <|> transVerbT <|> (singlet Root <++> kplus' (singlet Root <|> singlet Suffix))

-- phrase level --

uElid, suElid, guElid, kuElid, vuElid, xu :: Parser [Token] [Token]
uElid = optiont Transmogrifier
suElid = optiont Su
guElid = optiont SSTerm
kuElid = optiont LSTerm
vuElid = optiont DSTerm
xu = singlet AllTerm

illocutions, modifiers, quote :: Parser [Token] [Token]
illocutions =
  inContext "illocutions" $
    choice
      [ singlet Illocution,
        singlet DiscursiveIllocution <* optiont WS <++> singlet ModalIllocution,
        singlet ModalIllocution <* optiont WS <++> singlet DiscursiveIllocution
      ]
modifiers = inContext "modifiers" $ kplus' (singlet Modifier <* optiont WS)
quote = singlet Quote

adverbial, adverbPhrase, transAdverb, adverbs :: Parser [Token] [Token]
adverbial = inContext "adverbial" $ singlet Adverb <++> singlet AdverbSuffix <|> singlet Adverb <|> singlet Onomatopoeia
adverbPhrase =
  inContext "adverb phrase" $
    option' modifiers
      <++> adverbial
      <++> option' (optiont WS *> singlet Connective <* optiont WS <++> adverbPhrase)
transAdverb =
  inContext "transmogrified adverb" $
    optiont WS
      *> modifiers
        <& choice
          [ option' (singlet Connective) <* optiont WS <++> option' modifiers <++> singlet Illocution,
            singlet SSTerm,
            singlet LSTerm,
            singlet DSTerm,
            xu,
            singlet EOF
          ]
adverbs = inContext "adverbs" $ kplus' (adverbPhrase <* optiont WS) <|> transAdverb

freeNoun, freeConnectiveNoun :: Parser [Token] [Token]
freeNoun =
  inContext "free noun" $
    suElid
      <++> option' modifiers
      <++> (singlet Pronoun <|> quote)
      <* optiont WS <++> kstar' tagPhrase
      <* optiont WS
        <++> guElid
        <++> option' (optiont WS *> singlet Connective <* optiont WS <++> freeNoun)
freeConnectiveNoun =
  inContext "connected free nouns" $
    optiont WS *> singlet Connective <* optiont WS <++> freeNoun

termNucleus, predicateTerm, prepositionTerm, freeTerm, freeConnectiveTerm :: Parser [Token] [Token]
termNucleus =
  inContext "term nucleus" $
    optiont WS
      *> kstar' ((prepositionTerm <|> adverbs) <* optiont WS)
        <++> choice
          [ freeTerm,
            choice
              [ predicate,
                tagPhrase,
                binderPhrase,
                option' modifiers <++> choice [singlet Pronoun, quote, verbLT] <++> kstar' tagPhrase
              ]
              <* optiont WS
                <++> guElid
          ]
predicateTerm =
  inContext "predicate term" $
    choice
      [ optiont WS
          *> option' modifiers
            <++> singlet Transmogrifier
          <* optiont WS
            <++> (termNucleus <|> kstar' (choice [prepositionTerm, adverbs] <* optiont WS) <++> guElid),
        uElid
          <* optiont WS
            <++> choice [predicate, tagPhrase, binderPhrase]
          <* optiont WS
            <++> guElid
      ]
      <* optiont WS
        <++> option' (singlet Connective <* optiont WS <++> predicateTerm)
prepositionTerm =
  inContext "preposition term" $
    optiont WS
      *> option' modifiers
        <++> option' (singlet VerbModifier <* optiont WS)
        <++> choice
          [ singlet SSPrep
              <* optiont WS
                <++> (termNucleus <|> kstar' (choice [prepositionTerm, adverbs] <* optiont WS) <++> guElid),
            singlet SSPrep <* optiont WS <& singlet Illocution <++> discourse,
            singlet LSPrep <* optiont WS <++> clause <* optiont WS <++> kuElid
          ]
      <* optiont WS
        <++> option' (singlet Connective <* optiont WS <++> prepositionTerm)
freeTerm =
  inContext "free term" $
    optiont WS
      *> option' modifiers
        <++> choice
          [ kplus' (singlet SSDet <* optiont WS)
              <++> (termNucleus <|> kstar' (choice [prepositionTerm, adverbs] <* optiont WS) <++> guElid),
            kstar' (singlet SSDet <* optiont WS) <++> singlet LSDet <* optiont WS <++> clause <* optiont WS <++> kuElid,
            singlet SSDet <* optiont WS <& singlet Illocution <++> discourse
          ]
      <* optiont WS
        <++> option' (singlet Connective <* optiont WS <++> freeTerm)
freeConnectiveTerm =
  inContext "connected free terms" $
    optiont WS *> singlet Connective <* optiont WS <++> (prepositionTerm <|> freeTerm)

freeNominal, connectiveNominal, nounTerm, nounTerms, fragment :: Parser [Token] [Token]
freeNominal =
  inContext "free nominal" $
    (freeTerm <|> freeNoun)
      <++> option' (optiont WS *> singlet Connective <++> freeNominal)
connectiveNominal =
  inContext "free nominal" $
    (freeConnectiveTerm <|> freeConnectiveNoun)
      <++> option' (optiont WS *> singlet Connective <++> freeNominal)
nounTerm = inContext "noun term" $ prepositionTerm <|> freeNominal <|> connectiveNominal
nounTerms = inContext "noun terms" $ kplus' (nounTerm <* optiont WS)
fragment = inContext "fragment" $ kplus' (nounTerms <|> adverbs)

verbal, serial, predicate :: Parser [Token] [Token]
verbal =
  inContext "verbal" $
    option' (singlet VerbModifier <* optiont WS)
      <++> ( verbT <* optiont WS <++> kstar' binderPhrase
               <|> verbNT <* optiont WS <++> clause <* optiont WS <++> kuElid
           )
serial =
  inContext "serial" $
    option' modifiers
      <* optiont WS
        <++> (verbal <|> tagPhrase <|> binderPhrase)
        <++> kstar' (tagPhrase <|> binderPhrase)
        <++> kplus' (optiont WS *> (serial <|> option' modifiers <++> verbal))
predicate =
  inContext "predicate" $
    optiont WS
      *> option' modifiers
        <++> (serial <|> verbal)
        <++> kstar' tagPhrase
        <++> option' (optiont WS *> singlet Connective <* optiont WS <++> predicate)

frame, clause :: Parser [Token] [Token]
frame = inContext "frame" $ predicateTerm <++> option' fragment
clause =
  inContext "clause" $
    kstar' (fragment <++> option' (xu <* optiont WS))
      <++> kplus' frame
      <++> option' (optiont WS *> singlet Connective <* optiont WS <++> clause)

binderPhrase, tagPhrase :: Parser [Token] [Token]
binderPhrase =
  inContext "binder phrase" $
    optiont WS
      *> option' modifiers
        <++> choice
          [ singlet SSBinder
              <* optiont WS
                <++> option' adverbs
                <++> choice
                  [ termNucleus,
                    kstar' ((prepositionTerm <|> adverbs) <* optiont WS) <++> guElid
                  ],
            singlet LSBinder <* optiont WS <++> clause <* optiont WS <++> kuElid
          ]
        <++> option' (optiont WS *> singlet Connective <* optiont WS <++> binderPhrase)
tagPhrase =
  inContext "tag phrase" $
    optiont WS
      *> option' modifiers
        <++> choice
          [ singlet SSTag
              <* optiont WS
                <++> option' adverbs
                <++> choice
                  [ termNucleus,
                    kstar' ((prepositionTerm <|> adverbs) <* optiont WS) <++> guElid
                  ],
            singlet LSTag <* optiont WS <++> clause <* optiont WS <++> kuElid
          ]
        <++> option' (optiont WS *> singlet Connective <* optiont WS <++> tagPhrase)

-- text level --

sentence, discourse, text :: Parser [Token] [Token]
sentence =
  inContext "sentence" $
    choice
      [ option' illocutions
          <* optiont WS
            <++> kplus' (choice [clause, fragment] <++> option' (xu <* optiont WS)),
        illocutions
          <* optiont WS
            <& choice [singlet Illocution, singlet EOF, singlet DSTerm]
      ]
      <++> option'
        ( optiont WS
            *> singlet Connective
            <* optiont WS
              <& singlet Illocution
              <++> sentence
        )
discourse =
  inContext "discourse" $
    kplus' (optiont WS *> sentence)
      <* optiont WS
        <++> vuElid
        <++> option' (optiont WS *> singlet Connective <* optiont WS <++> discourse)
text = inContext "text" $ optiont WS *> kstar' (optiont WS *> discourse)
