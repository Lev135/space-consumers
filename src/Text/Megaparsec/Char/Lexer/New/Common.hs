{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
module Text.Megaparsec.Char.Lexer.New.Common where

import Control.Monad (unless, void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Monoid as Monoid
import qualified Data.Set as E
import Text.Megaparsec
import Text.Megaparsec.Char (eol)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)

-- | Parse a non-indented construction. This ensures that there is no
-- indentation before actual data. Useful, for example, as a wrapper for
-- top-level function definitions.
nonIndented :: (TraversableStream s, MonadParsec e s m) =>
  m a -> m a
nonIndented = (L.indentGuard (pure ()) EQ pos1 *>)
{-# INLINEABLE nonIndented #-}

-- | A type synonym for space and eol consumer. Should be called manually
-- wherever line break is expected
type Scn m
  = m ()

-- | Generalized version of `block`, providing a way to change what is desired
-- ordering related to a reference indentation level
blockWith :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => Ordering -- ^ desired ordering @act \`compare\` ref@
  -> Pos -- ^ reference indentation level
  -> Scn m -- ^ space and eols consumer
  -> (Scn m -> m a) -- ^ callback that uses provided space consumer
  -> m a -- ^ result returned by a callback
blockWith ord ref scn action =
  action $ void $ L.indentGuard (eol *> scn) ord ref
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
block :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
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
-- prop> optionalBody = optionBody Nothing (fmap Just . f)
optionalBody :: Functor m => (Scn m -> m a) -> Body m (Maybe a)
optionalBody f = BodyOpt Nothing (fmap Just . f)

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
headedBlock :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
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

headedOne :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m (el -> a) -- ^ how to parse a head
  -> (Scn m -> m el) -- ^ callback to parse a body
  -> m a -- callback's result transformed by the result of head parser
headedOne hscn scn pHead pEl = headedBlock hscn scn $
  BodyOne . (\f -> fmap f . pEl) <$> pHead
{-# INLINEABLE headedOne #-}

headedOptional :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m (Maybe el -> a) -- ^ how to parse a head
  -> (Scn m -> m el) -- ^ callback to parse a body
  -> m a -- callback's result transformed by the result of head parser
headedOptional hscn scn pHead pEl = headedBlock hscn scn do
  h <- pHead
  pure $ BodyOpt (h Nothing) (fmap (h . Just) . pEl)
{-# INLINEABLE headedOptional #-}

headedSome :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m ([el] -> a) -- ^ how to parse a head
  -> m el -- ^ how to parse each element of the body
  -> m a -- result of the head parser, applied to parsed elements of body
headedSome hscn scn pHead pEl = headedBlock hscn scn $
  fmap (<$> someBody pEl) pHead
{-# INLINEABLE headedSome #-}

headedMany :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => Scn m -- ^ how to consume white space after the head
  -> Scn m -- ^ how to consume white space after each line of body
  -> m ([el] -> a) -- ^ how to parse a head
  -> m el -- ^ how to parse each element of the body
  -> m a -- result of the head parser, applied to parsed elements of body
headedMany hscn scn pHead pEl = headedBlock hscn scn $
  fmap (<$> manyBody pEl) pHead
{-# INLINEABLE headedMany #-}

data LineFoldErrInfo = LineFoldErrInfo Int Ordering Pos Pos
  deriving (Read, Show)

replaceLineFoldError :: MonadParsec e s m => m a -> m a
replaceLineFoldError = region procErr
  where
    procETok (Label lbl) = case readMaybe (NE.toList lbl) of
      (Just (LineFoldErrInfo o ord ref act)) ->
          Just $ FancyError o $ E.singleton $ ErrorIndentation ord ref act
      _ -> Nothing
    procETok _ = Nothing
    procErr e = case e of
          TrivialError _ _ etoks ->
            case Monoid.getFirst $ foldMap (Monoid.First . procETok) etoks of
              (Just e') -> e'
              Nothing   -> e
          _ -> e

lineFoldWith :: (TraversableStream s, MonadParsec e s m, MonadParsec e s n) =>
  Ordering -> Pos -> n () -> Scn n -> (n () -> m a) -> m a
lineFoldWith ord ref sc scn action =
  replaceLineFoldError . action . void $ sc *> (optional . try) do
    st <- getParserState
    lvl' <- scn *> L.indentLevel
    unless (lvl' `compare` ref == ord) do
      o <- getOffset
      setParserState st
      let i = LineFoldErrInfo o ord ref lvl'
      failure Nothing (E.singleton $ Label $ NE.fromList $ show i)
{-# INLINEABLE lineFoldWith #-}
