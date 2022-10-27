{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Text.Megaparsec.Char.Lexer.New
-- Copyright   :  (c) 2022 Lev Dvorkin
-- License     :  BSD3
--
-- Maintainer  :  Lev Dvorkin <lev_135@mail.ru>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides alternative to /Text.Megaparsec.Char.Lexer/ approach
-- for lexing process, especially for indentation-sensitive parsing.
-- It's highly not recommended to mix functions from these two modules.
--
-- Parsing of white space is an important part of any parser. We propose
-- a special policy for consuming line spaces (i. e. spaces and tabs) and line
-- endings:
--
-- - For line spaces we follow /Text.Megaparsec.Char.Lexer/: each lexeme should
--   consume __all line spaces after it__ and this can be done by wrapping it in
--   `lexeme` combinator.
-- - For end of line symbols we have a different convention: parser should
--   consume __only eols inside it's block__, but __not those, which follow it__.
--
-- Also note that you need to call 'space' manually to consume any white space
-- before the first lexeme (i.e. at the beginning of the file).
--
-- This module is intended to be imported qualified:
--
-- > import qualified Text.Megaparsec.Char.Lexer.New as L
--

module Text.Megaparsec.Char.Lexer.New
  ( -- * Space consumer wrappers
    Sc (..), C.Scn,
    -- * White space
    L.space,
    lexeme,
    symbol,
    symbol',
    L.skipLineComment,
    L.skipBlockComment,
    L.skipBlockCommentNested,

    -- * Indentation
    -- ** Primitives for indentation-sensitive parsing
    L.indentLevel,
    L.incorrectIndent,
    L.indentGuard,
    C.nonIndented,
    -- ** Blocks of line
    C.block,
    C.blockWith,
    -- ** Headed blocks
    -- *** Simple combinators
    C.headedOne,
    C.headedOptional,
    C.headedSome,
    C.headedMany,
    -- *** General combinators
    C.headedBlock,
    C.Body,
    C.pureBody,
    C.oneBody,
    C.optionBody,
    C.optionalBody,
    C.someBody,
    C.manyBody,
    -- ** Line folds
    C.replaceLineFoldError,
    lineFold,
    paragraph,
    lineFoldWith,

    -- * Character and string literals
    L.charLiteral,

    -- * Numbers
    L.decimal,
    L.binary,
    L.octal,
    L.hexadecimal,
    L.scientific,
    L.float,
    L.signed,
  ) where

import Data.CaseInsensitive (FoldCase)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char.Lexer.New.Common as C


-- | Newtype wrapper for line space consumer. In common cases you should use
-- standard combinators from /Text.Megaparsec.Char.Lexer.New/ rather than
-- unwrap it manually
newtype Sc m
  = Sc { unSc :: m () }

-- | A @lexeme sc p@ behaves like @p@ and consumes spaces by @sc@ after it
lexeme :: MonadParsec e s m =>
  Sc m -> m a -> m a
lexeme sc = L.lexeme $ unSc sc
{-# INLINEABLE lexeme #-}

-- | @symbol sc toks@ parse toks and consumes spaces by @sc@ after them
symbol :: MonadParsec e s m =>
  Sc m -> Tokens s -> m (Tokens s)
symbol sc = L.symbol $ unSc sc
{-# INLINEABLE symbol #-}

-- | A case-insensitive version of `symbol`
symbol' :: (MonadParsec e s m, FoldCase (Tokens s)) =>
  Sc m -> Tokens s -> m (Tokens s)
symbol' sc = L.symbol' $ unSc sc
{-# INLINEABLE symbol' #-}

lineFoldWith :: (TraversableStream s, MonadParsec e s m) =>
  Ordering -> Pos -> Sc m -> C.Scn m -> (Sc m -> m a) -> m a
lineFoldWith ord ref sc scn action =
  C.lineFoldWith ord ref (unSc sc) scn (action . Sc)
{-# INLINEABLE lineFoldWith #-}

lineFold ::
  (TraversableStream s, MonadParsec e s m) =>
  -- | Line space consumer
  Sc m ->
  -- | Line space and eols consumer
  C.Scn m ->
  -- | Callback that uses provided space-consumer
  (Sc m -> m a) ->
  m a
lineFold sc scn action = do
  lvl <- L.indentLevel
  lineFoldWith GT lvl sc scn action
{-# INLINEABLE lineFold #-}

paragraph ::
  (TraversableStream s, MonadParsec e s m) =>
  -- | Line space consumer
  Sc m ->
  -- | Line space and eols consumer
  C.Scn m ->
  -- | Callback that uses provided space-consumer
  (Sc m -> m a) ->
  m a
paragraph sc scn action = do
  lvl <- L.indentLevel
  lineFoldWith EQ lvl sc scn action
{-# INLINEABLE paragraph #-}
