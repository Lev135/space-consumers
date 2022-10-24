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
    -- ** Primitives for indentation-sensitive parsing
    L.indentLevel,
    L.incorrectIndent,
    L.indentGuard,
    -- ** Blocks of line
    block,
    blockWith,
    -- ** Headed blocks
    -- *** Simple combinators
    headedOne,
    headedOptional,
    headedSome,
    headedMany,
    -- *** General combinators
    headedBlock,
    Body,
    pureBody,
    oneBody,
    optionBody,
    optionalBody,
    someBody,
    manyBody,
    -- ** Line folds
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
-- unwrap it manually
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

-- | Generalized version of `block`, providing a way to change what is desired
-- ordering related to a reference indentation level
blockWith :: (TraversableStream s, MonadParsec e s m)
  => Ordering -- ^ desired ordering @act \`compare\` ref@
  -> Pos -- ^ reference indentation level
  -> Scn m -- ^ space and eols consumer
  -> (Scn m -> m a) -- ^ callback that uses provided space consumer
  -> m a -- ^ result returned by a callback
blockWith ord ref scn action =
  action $ void $ L.indentGuard scn ord ref
{-# INLINEABLE blockWith #-}

-- | Parse a block of consecutive lines with the same Indentation
--
-- For example, for parsing something like
--
-- @
-- foo
-- bar
-- baz
-- @
--
-- you can use something like this
--
-- @
-- block space $ \scn -> do
--   string "foo" <* scn
--   string "bar" <* scn
--   string "baz" -- we do not use eol consumer after the last string!
-- @
block :: (TraversableStream s, MonadParsec e s m)
  => Scn m -> (Scn m -> m a) -> m a
block scn action = do
  ref <- L.indentLevel
  blockWith EQ ref scn action
{-# INLINEABLE block #-}

-- | Opaque type, containing information about parsing `headedBlock`s body
--
-- `Functor` instance can be used to modify a result value
data Body m a
  = BodyNone a
  | BodyOne (Scn m -> m a)
  | BodyOpt a (Scn m -> m a)
  deriving (Functor)

-- | Don't parse anything, just return a constant value. Can be used if after
-- parsing head you realized that there should be no body here
pureBody :: a -> Body m a
pureBody = BodyNone

-- | Parse a body by given callback. Callback can use space consumer to
-- parse a multiline body. If the body consists of some identical parts use
-- `someBody`/`manyBody` instead.
--
-- Note, that it will always fail, if the body is empty, even if a callback can
-- succeed without consuming input. Use `optionBody`/`optionalBody` in this case.
oneBody :: (Scn m -> m a) -> Body m a
oneBody = BodyOne

-- | Parse a body by given callback, if the next line after a head has greater
-- indentation level, otherwise return a constant value
optionBody :: a -> (Scn m -> m a) -> Body m a
optionBody = BodyOpt

-- | Parse a body by given callback, if the next line after a head has greater
-- indentation level, otherwise return Nothing
--
-- prop> optionalBody = optionBody Nothing
optionalBody :: (Scn m -> m (Maybe a)) -> Body m (Maybe a)
optionalBody = BodyOpt Nothing

-- | Parse some (greater, than zero) number of lines by given parser
--
-- prop> someBody pEl = oneBody (pEl `sepBy1`)
someBody :: (TraversableStream s, MonadParsec e s m)
  => m el -> Body m [el]
someBody pEl = BodyOne (pEl `sepBy1`)
{-# INLINEABLE someBody #-}

-- | Parse many (maybe zero) lines by given parser
--
-- prop> manyBody pEl = optionBody [] (pEl `sepBy`)
manyBody :: (TraversableStream s, MonadParsec e s m)
  => m el -> Body m [el]
manyBody pEl = BodyOpt [] (pEl `sepBy`)
{-# INLINEABLE manyBody #-}

-- | Parse a head of the block and then its body, depending on what `Body`
-- is returned after processing of the head. Use it if the choice of body parser
-- depends on the head parser's result. In other cases you should prefer
-- `headedOne`/`headedSome`/`headedMany`/`headedOptional`
--
-- For example, suppose we want to parse something like this (in the first line
-- we have an arbitrary identifier and than it's repeated in all subsequent
-- lines):
--
-- @
-- foo:
--   foo 42
--   foo 36
-- @
--
-- we can use something like this:
--
-- @
-- headedBlock space space $ do
--   name <- takeWhile1P isLetter
--   string ":"
--   pure $ someBody (L.symbol hspace name *> L.decimal)
-- @
headedBlock :: (TraversableStream s, MonadParsec e s m)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m (Body m a) -- ^ how to parse a head and get body parser
  -> m a -- ^ the value, returned by body parser
headedBlock hscn scn pContent = do
  ref <- L.indentLevel
  content <- pContent
  case content of
    BodyNone a -> pure a
    BodyOne pa -> do
      lvl <- L.indentGuard hscn GT ref
      blockWith EQ lvl scn pa
    BodyOpt a pa -> do
      lvl <- hscn *> L.indentLevel
      if lvl > ref
        then blockWith EQ lvl scn pa
        else pure a
{-# INLINEABLE headedBlock #-}

headedOne :: (TraversableStream s, MonadParsec e s m)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m (el -> a) -- ^ how to parse a head
  -> (Scn m -> m el) -- ^ callback to parse a body
  -> m a -- callback's result transformed by the result of head parser
headedOne hscn scn pHead pEl = headedBlock hscn scn $
  BodyOne . (\f -> fmap f . pEl) <$> pHead
{-# INLINEABLE headedOne #-}

headedOptional :: (TraversableStream s, MonadParsec e s m)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m (Maybe el -> a) -- ^ how to parse a head
  -> (Scn m -> m el) -- ^ callback to parse a body
  -> m a -- callback's result transformed by the result of head parser
headedOptional hscn scn pHead pEl = headedBlock hscn scn do
  h <- pHead
  pure $ BodyOpt (h Nothing) (fmap (h . Just) . pEl)
{-# INLINEABLE headedOptional #-}

headedSome :: (TraversableStream s, MonadParsec e s m)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m ([el] -> a) -- ^ how to parse a head
  -> m el -- ^ how to parse each element of the body
  -> m a -- result of the head parser, applied to parsed elements of body
headedSome hscn scn pHead pEl = headedBlock hscn scn $
  fmap (<$> someBody pEl) pHead
{-# INLINEABLE headedSome #-}

headedMany :: (TraversableStream s, MonadParsec e s m)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m ([el] -> a) -- ^ how to parse a head
  -> m el -- ^ how to parse each element of the body
  -> m a -- result of the head parser, applied to parsed elements of body
headedMany hscn scn pHead pEl = headedBlock hscn scn $
  fmap (<$> manyBody pEl) pHead
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

paragraph ::
  (TraversableStream s, MonadParsec e s m) =>
  -- | Line space consumer
  Sc m ->
  -- | Line space and eols consumer
  Scn m ->
  -- | Callback that uses provided space-consumer
  (Sc m -> m a) ->
  m a
paragraph sc scn action = do
  lvl <- L.indentLevel
  lineFoldWith EQ lvl sc scn action
{-# INLINEABLE paragraph #-}
