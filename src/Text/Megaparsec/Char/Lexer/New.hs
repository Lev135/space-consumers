{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Text.Megaparsec.Char.Lexer.New
-- Copyright   :  (c) 2022 Lev Dvorkin
-- License     :  FreeBSD
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
    Sc (..), Scn,
    -- * White space
    L.space,
    lexeme,
    symbol,
    symbol',
    L.skipLineComment,
    L.skipBlockComment,
    L.skipBlockCommentNested,

    -- * Indentation
    L.indentLevel,
    L.incorrectIndent,
    L.indentGuard,
    blockWith,
    block,
    Block (..),
    blockSome,
    blockMany,
    headedBlock,
    headedOne,
    headedMay,
    headedSome,
    headedMany,
    lineFoldWith,
    lineFold,

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

import Control.Monad (unless, void)
import Data.CaseInsensitive (FoldCase)
import qualified Data.List.NonEmpty as NE
import qualified Data.Monoid as Monoid
import qualified Data.Set as E
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)

-- | Newtype wrapper for line space consumer. In common cases you should use
-- standard combinators from /Text.Megaparsec.Char.Lexer.New/ rather than
-- unwrapping it manually
newtype Sc m
  = Sc { unSc :: m () }

-- | A type synonym for space and eol consumer. Should be called manually
-- wherever line break is expected
type Scn m
  = m ()

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


blockWith :: (TraversableStream s, MonadParsec e s m) =>
  Ordering -> Pos -> Scn m -> (Scn m -> m a) -> m a
blockWith ord ref scn action =
  action $ void $ L.indentGuard scn ord ref
{-# INLINEABLE blockWith #-}

{- |
@
  foo
  bar
  baz
@
-}
block :: (TraversableStream s, MonadParsec e s m) =>
  Scn m -> (Scn m -> m a) -> m a
block scn action = do
  ref <- L.indentLevel
  blockWith EQ ref scn action
{-# INLINEABLE block #-}

data Block m a
  = BlockNone a
  | BlockOne (Scn m -> m a)
  | BlockMay a (Scn m -> m a)
  deriving (Functor)

{- |
@
foo:
  bar
  baz
@
-}
headedBlock :: (TraversableStream s, MonadParsec e s m) =>
  Scn m -> Scn m -> m (Block m a) -> m a
headedBlock hscn scn pContent = do
  ref <- L.indentLevel
  content <- pContent
  case content of
    BlockNone a -> pure a
    BlockOne pa -> do
      lvl <- L.indentGuard hscn GT ref
      blockWith EQ lvl scn pa
    BlockMay a pa -> do
      lvl <- hscn *> L.indentLevel
      if lvl > ref
        then blockWith EQ lvl scn pa
        else pure a
{-# INLINEABLE headedBlock #-}

headedOne :: (TraversableStream s, MonadParsec e s m)
  => Scn m -> Scn m -> m (el -> a) -> (Scn m -> m el) -> m a
headedOne hscn scn pHead pEl = headedBlock hscn scn $
  BlockOne . (\f -> fmap f . pEl) <$> pHead
{-# INLINEABLE headedOne #-}

headedMay :: (TraversableStream s, MonadParsec e s m) =>
  Scn m -> Scn m -> m (Maybe el -> a) -> (Scn m -> m el) -> m a
headedMay hscn scn pHead pEl = headedBlock hscn scn do
  h <- pHead
  pure $ BlockMay (h Nothing) (fmap (h . Just) . pEl)
{-# INLINEABLE headedMay #-}

blockSome :: (TraversableStream s, MonadParsec e s m) =>
  m el -> Block m [el]
blockSome pEl = BlockOne \scn' -> pEl `sepBy1` scn'
{-# INLINEABLE blockSome #-}

blockMany :: (TraversableStream s, MonadParsec e s m) =>
  m el -> Block m [el]
blockMany pEl = BlockMay [] \scn' -> pEl `sepBy` scn'
{-# INLINEABLE blockMany #-}

headedSome :: (TraversableStream s, MonadParsec e s m)
  => Scn m -> Scn m -> m ([el] -> a) -> m el -> m a
headedSome hscn scn pHead pEl = headedBlock hscn scn $
  fmap (<$> blockSome pEl) pHead
{-# INLINEABLE headedSome #-}

headedMany :: (TraversableStream s, MonadParsec e s m)
  => Scn m -> Scn m -> m ([el] -> a) -> m el -> m a
headedMany hscn scn pHead pEl = headedBlock hscn scn $
  fmap (<$> blockMany pEl) pHead
{-# INLINEABLE headedMany #-}

data LineFoldErrInfo = LineFoldErrInfo Int Pos
  deriving (Read, Show)

lineFoldWith :: (TraversableStream s, MonadParsec e s m) =>
  Ordering -> Pos -> Sc m-> Scn m -> (Sc m -> m a) -> m a
lineFoldWith ord ref sc scn action =
  region procErr $
    action . Sc . void $ unSc sc *> (optional . try) do
      st <- getParserState
      lvl' <- scn *> L.indentLevel
      unless (lvl' `compare` ref == ord) do
        o <- getOffset
        setParserState st
        let i = LineFoldErrInfo o lvl'
        failure Nothing (E.singleton $ Label $ NE.fromList $ show i)
  where
    procETok (Label lbl) = case readMaybe (NE.toList lbl) of
      (Just (LineFoldErrInfo o act)) ->
          Just $ FancyError o $ E.singleton $ ErrorIndentation GT ref act
      _ -> Nothing
    procETok _ = Nothing
    procErr e = case e of
          TrivialError _ _ etoks ->
            case Monoid.getFirst $ foldMap (Monoid.First . procETok) etoks of
              (Just e') -> e'
              Nothing   -> e
          _ -> e
{-# INLINEABLE lineFoldWith #-}

lineFold ::
  (TraversableStream s, MonadParsec e s m) =>
  -- | Line space consumer
  Sc m ->
  -- | Line space and eols consumer
  Scn m ->
  -- | Callback that uses provided space-consumer
  (Sc m -> m a) ->
  m a
lineFold sc scn action = do
  lvl <- L.indentLevel
  lineFoldWith GT lvl sc scn action
{-# INLINEABLE lineFold #-}
