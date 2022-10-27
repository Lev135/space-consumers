{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import Control.Monad.Combinators.Expr (Operator(InfixL), makeExprParser)
import Data.Char (isAlpha)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer.New (Scn)
import qualified Text.Megaparsec.Char.Lexer.New as L
import Text.Megaparsec.Char.Lexer.Stateful (runScT)
import qualified Text.Megaparsec.Char.Lexer.Stateful as SL

type Parser = Parsec Void String

sc :: L.Sc Parser
sc = L.Sc hspace

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
  |        ^^
unexpected "ba"
expecting end of line or white space
-}

ex1_1 :: Parser [(String, Int)]
ex1_1 = ex1 space

{- | This one will not accept empty lines:

>>> parseTest ex1_2 "foo 42\n\nbar 1024"
2:1:
  |
2 | <empty line>
  | ^^^
unexpected "<newline>ba"
expecting "bar" or white space
-}
ex1_2 :: Parser [(String, Int)]
ex1_2 = ex1 hspace

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
  pure $ L.oneBody \scn' -> do
    _ <- L.symbol sc name1
    val1 <- L.lexeme sc L.decimal
    scn'
    _ <- L.symbol sc name2
    val2 <- L.lexeme sc L.decimal
    pure [(name1, val1), (name2, val2)]

ex_fold :: Parser [String]
ex_fold = L.lineFold sc space \sc' -> do
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
ex_fold2 = L.lineFold sc space \sc' -> do
  a <- L.symbol sc' "a"
  bs <- some $ L.symbol sc' "b"
  cs <- many $ L.symbol sc' "c"
  pure $ a : bs <> cs

data Exp
  = Lit Int
  | Sum Exp Exp
  | Prod Exp Exp
instance Show Exp where
  show (Lit n)     = show n
  show (Sum e e')  = "(" <> show e <> " + " <> show e' <> ")"
  show (Prod e e') = "(" <> show e <> " * " <> show e' <> ")"

ex_foldExp :: Parser Exp
ex_foldExp = L.lineFold sc space pExp
  where
    pExp sc' = makeExprParser (pTerm sc') (operators sc')
    pTerm sc' = Lit <$> L.lexeme sc' L.decimal
      <|> L.symbol sc' "(" *> pExp sc' <* L.symbol sc' ")"
    operators sc' =
      [ [InfixL (Prod <$ L.symbol sc' "*")]
      , [InfixL (Sum <$ L.symbol sc' "+")]
      ]
{-
- Everything is ok:
>>> parseTest (ex_foldExp <* eof) "1\n + 2 + \n 3"
((1 + 2) + 3)

- Good error message:
>>> parseTest (ex_foldExp <* eof) "1\n + 2 + \n3"
3:1:
  |
3 | 3
  | ^
incorrect indentation (got 1, should be greater than 1)

- Very bad error message:
>>> parseTest (ex_foldExp <* eof) "1\n+ 2 + \n 3"
1:2:
  |
1 | 1
  |  ^
unexpected newline
expecting '*', '+', LineFoldErrInfo 2 (Pos 1), digit, end of input, or white space

- Dealing with bad errors by wrapping in `L.replaceLineFoldError`
>>> parseTest (L.replaceLineFoldError $ ex_foldExp <* eof) "1\n+ 2 + \n 3"
2:1:
  |
2 | + 2 +
  | ^
incorrect indentation (got 1, should be greater than 1)

-}

ex_foldExp' :: Parser Exp
ex_foldExp' = SL.lineFold space pExp `runScT` hspace
  where
    pExp = makeExprParser pTerm operators
    pTerm = Lit <$> SL.lexeme L.decimal
      <|> SL.symbol "(" *> pExp <* SL.symbol ")"
    operators =
      [ [InfixL (Prod <$ SL.symbol "*")]
      , [InfixL (Sum <$ SL.symbol "+")]
      ]



main :: IO ()
main = pure ()
