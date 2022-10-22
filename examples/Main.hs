{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import Data.Char (isAlpha)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer.New as L
import Text.Megaparsec.Char.Lexer.New (Scn)

type Parser = Parsec Void String

sc :: L.Sc Parser
sc = L.Sc hspace

scn, scn1, scn11 :: Parser ()
-- | Consumes line space and eols, possibly nothing
scn = hidden space
-- | Consumes line space and eols, including at least one eol
scn1 = label "end of line" $ hspace *> eol *> hidden space
-- | Consumes line space and exactly one eol
scn11 = label "end of line" $ hspace *> eol *> hidden hspace

-- | The first example.
ex1 :: Scn Parser -> Parser [(String, Int)]
ex1 scn = L.block scn \scn' -> do
  foo <- L.symbol sc "foo"
  fooN <- L.lexeme sc L.decimal
  scn'
  bar <- L.symbol sc "bar"
  barN <- L.lexeme sc L.decimal
  pure [(foo, fooN), (bar, barN)]

{- | Parses block with two lines:

>>> parseTest ex1_1 "foo 42\nbar 1024"
[("foo",42),("bar",1024)]

However, it gives a very bad error message if we forgot to break line:

>>> parseTest ex1_1 "foo 42 bar 1024"
1:8:
  |
1 | foo 42 bar 1024
  |        ^
incorrect indentation (got 8, should be equal to 1)

-}
ex1_1 :: Parser [(String, Int)]
ex1_1 = ex1 scn

{- | The error message for forgotten line break is much more accurate:

>>> parseTest ex1_2 "foo 42 bar 1024"
1:8:
  |
1 | foo 42 bar 1024
  |        ^^
unexpected "ba"
expecting end of line or white space
-}
ex1_2 :: Parser [(String, Int)]
ex1_2 = ex1 scn1

{- | This one will not accept empty lines:

>>> parseTest ex1_2 "foo 42\n\nbar 1024"
2:1:
  |
2 | <empty line>
  | ^^^
unexpected "<newline>ba"
expecting "bar"
-}
ex1_3 :: Parser [(String, Int)]
ex1_3 = ex1 scn11

{-
foo&bar:
  foo 42
  bar 1024
-}
ex2 :: Scn Parser -> Scn Parser -> Parser [Int]
ex2 hscn scn = L.headedOne hscn scn h \scn' -> do
  _ <- L.symbol sc "foo"
  fooN <- L.lexeme sc L.decimal
  scn'
  _ <- L.symbol sc "bar"
  barN <- L.lexeme sc L.decimal
  pure [fooN, barN]
  where
    h = do
      _ <- L.symbol sc "foo"
      _ <- L.symbol sc "&"
      _ <- L.symbol sc "bar"
      _ <- L.symbol sc ":"
      pure id


{-
foo&bar:
  foo 42
  bar 1024

bar & baz:
  bar 13
  baz 12
-}
ex3 :: Scn Parser -> Scn Parser -> Parser [(String, Int)]
ex3 hscn scn = L.headedBlock hscn scn do
  name1 <- label "first name" $ L.lexeme sc $ takeWhile1P Nothing isAlpha
  _ <- L.symbol sc "&"
  name2 <- label "second name" $ L.lexeme sc $ takeWhile1P Nothing isAlpha
  _ <- L.symbol sc ":"
  pure $ L.BlockOne \scn' -> do
    _ <- L.symbol sc name1
    val1 <- L.lexeme sc L.decimal
    scn'
    _ <- L.symbol sc name2
    val2 <- L.lexeme sc L.decimal
    pure [(name1, val1), (name2, val2)]

ex_fold :: Parser [String]
ex_fold = L.lineFold sc scn \sc' -> do
  a <- L.symbol sc' "a"
  b <- L.symbol sc' "b"
  c <- L.symbol sc' "c"
  pure [a, b, c]
{-
>>> parseTest ex_fold "a b c"
["a","b","c"]
>>> parseTest ex_fold "a\n b\n c"
["a","b","c"]
>>> parseTest ex_fold "a\n b\n c\nd"
["a","b","c"]
>>> parseTest ex_fold "a\nb\n c\nd"
2:1:
  |
2 | b
  | ^
incorrect indentation (got 1, should be greater than 1)
-}

ex_fold2 :: Parser [String]
ex_fold2 = L.lineFold sc scn \sc' -> do
  a <- L.symbol sc' "a"
  bs <- some $ L.symbol sc' "b"
  cs <- many $ L.symbol sc' "c"
  pure $ a : bs <> cs

main :: IO ()
main = pure ()
