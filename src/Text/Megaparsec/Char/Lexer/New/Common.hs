{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
module Text.Megaparsec.Char.Lexer.New.Common where

import Control.Monad (unless, void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Monoid as Monoid
import qualified Data.Set as E
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)

-- | Newtype wrapper for line space consumer.
--
-- In common cases you should use standard combinators rather than unwrap it manually
newtype Sc m
  = Sc { unSc :: m () }

-- | Construct a line space consumer
makeSc :: MonadParsec e s m =>
  m () -- ^ how to consume some line spaces (at least one)
  -> m () -- ^ how to consume a line comment
  -> Sc m
makeSc spaceC lineCommentC = Sc $ L.space spaceC lineCommentC empty

-- | Newtype wrapper for line space & eols consumer. Can consume no eols at all.
--
-- In common cases you should use standard combinators rather than unwrap it manually
newtype Scn m
  = Scn { unScn :: m () }

-- | Construct an eol consumer
makeScn :: MonadParsec e s m =>
  m () -- ^ how to consume some line spaces (at least one)
  -> m () -- ^ how to consume some eols (at least one)
  -> m () -- ^ how to consume a line comment
  -> m () -- ^ how to consume a block comment
  -> Scn m
makeScn spaceC eolC lineCommentC blockCommentC =
  Scn $ L.space (spaceC <|> eolC) lineCommentC blockCommentC

-- | Newtype wrapper for line space & eols consumer. Must consume at least one
-- eol or ensure that we are at the end of input.
--
-- In common cases you should use standard combinators rather than unwrap it manually
newtype Scn1 m
  = Scn1 { unScn1 :: m () }

-- | Construct an eol consumer
makeScn1 :: MonadParsec e s m =>
  m () -- ^ how to consume some line spaces (at least one)
  -> m () -- ^ how to consume some eols (at least one)
  -> m () -- ^ how to consume a line comment
  -> m () -- ^ how to consume a block comment
  -> Scn1 m
makeScn1 spaceC eolC lineCommentC blockCommentC =
  Scn1 $ unSc (makeSc spaceC lineCommentC)
          *> (eolC <|> eof)
          *> unScn (makeScn spaceC eolC lineCommentC blockCommentC)

-- | Construct all space consumers at once
makeScs :: MonadParsec e s m =>
  m () -- ^ how to consume some line spaces (at least one)
  -> m () -- ^ how to consume some eols (at least one)
  -> m () -- ^ how to consume a line comment
  -> m () -- ^ how to consume a block comment
  -> (Sc m, Scn m, Scn1 m)
makeScs spaceC eolC lineCommentC blockCommentC =
  ( Sc sc, Scn scn, Scn1 scn1 )
    where
      sc = L.space spaceC lineCommentC empty
      scn = L.space (spaceC <|> eolC) lineCommentC blockCommentC
      scn1 = sc *> (eolC <|> eof) *> scn

-- | @line scn1 p@ behaves like @p@ but consumes at least one @eol@ after
line :: MonadParsec e s m => Scn1 m -> m a -> m a
line (Scn1 scn1) pa = pa <* scn1

-- | @mayLine scn p@ behaves like @p@ but consumes @eols@ after if they are there
mayLine :: MonadParsec e s m => Scn m -> m a -> m a
mayLine (Scn scn) pa = pa <* scn

-- | @Guard m@ is just a synonym to @m ()@. However, we provide it here to
-- establish one important invariant of all guards: they __never__ consume any
-- input so, to prevent loosing useful error message you __should not__ wrap
-- `Guard` call into `try` combinator.
type Guard m = m ()

-- | @indentGuard ord ref@ checks if actual indentation @act@ is acceptable,
-- i. e. @act `compare` ref == ord@ and we are not at the end of input.
-- Produces an `IncorrectIndent` error otherwise if the first fails and
-- "unexpected end of input" if the second fails.
--
-- If for some reasons you need to check that indentation of the last line in
-- file, write your one combinator using `indentLevel` and `incorrectIndent`
-- or use a standard `indentGuard` from `Text.Megaparsec.Char.Lexer`
--
-- Note, that we assume, that all white spaces have already been consumed
indentGuard :: (TraversableStream s, MonadParsec e s m) =>
  Ordering -> Pos -> Guard m
indentGuard ord ref = void $
  L.indentGuard (pure ()) ord ref <* notFollowedBy eof

-- | Parse a non-indented construction. This ensures that there is no
-- indentation before actual data. Useful, for example, as a wrapper for
-- top-level function definitions.
--
-- Note, that we assume, that all white spaces have already been consumed
nonIndented :: (TraversableStream s, MonadParsec e s m) =>
  m a -> m a
nonIndented = (L.indentGuard (pure ()) EQ pos1 *>)
{-# INLINEABLE nonIndented #-}

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
-- block $ \grd -> do
--   line space $ string "foo"
--   grd
--   line space $ string "bar"
--   grd
--   line space $ string "baz"
-- @
block :: (TraversableStream s, MonadParsec e s m)
  => (Guard m -> m a) -> m a
block action = L.indentLevel >>= \lvl -> action (indentGuard EQ lvl)
{-# INLINEABLE block #-}

-- | Opaque type, containing information about parsing `headedBlock`s body
--
-- `Functor` instance can be used to modify a result value
data Body m a
  = BodyNone a
  | BodyOne (Guard m -> m a)
  | BodyOpt a (Guard m -> m a)
  deriving (Functor)

-- | Don't parse anything, just return a constant value. Can be used if after
-- parsing head you realized that there should be no body here
pureBody :: a -> Body m a
pureBody = BodyNone

-- | Parse a body by given callback. Callback should check indentation level
-- using `Guard` at the beginning of each line. If the body consists of some
-- identical parts use `someBody`/`manyBody` instead.
--
-- Note, that it will always fail, if the body is empty, even if a callback can
-- succeed without consuming input. Use `optionBody`/`optionalBody` in this case.
oneBody :: (Guard m -> m a) -> Body m a
oneBody = BodyOne

-- | Parse a body by given callback, if the next line after a head has greater
-- indentation level, otherwise return a constant value
optionBody :: a -> (Guard m -> m a) -> Body m a
optionBody = BodyOpt

-- | Parse a body by given callback, if the next line after a head has greater
-- indentation level, otherwise return Nothing
--
-- prop> optionalBody = optionBody Nothing (fmap Just . f)
optionalBody :: Functor m => (Guard m -> m a) -> Body m (Maybe a)
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
  => m (Body m a) -- ^ how to parse a head and get body parser
  -> m a -- ^ the value, returned by body parser
headedBlock pContent = do
  ref <- L.indentLevel
  content <- pContent
  case content of
    BodyNone a -> pure a
    BodyOne pa -> do
      lvl <- L.indentLevel <* indentGuard GT ref
      pa (indentGuard EQ lvl)
    BodyOpt a pa ->
      option a do
        lvl <- L.indentLevel <* indentGuard GT ref
        pa (indentGuard EQ lvl)
{-# INLINEABLE headedBlock #-}

headedOne :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => m (el -> a) -- ^ how to parse a head
  -> (Guard m -> m el) -- ^ callback to parse a body
  -> m a -- callback's result transformed by the result of head parser
headedOne pHead pEl = headedBlock $
  BodyOne . (\f -> fmap f . pEl) <$> pHead
{-# INLINEABLE headedOne #-}

headedOptional :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => m (Maybe el -> a) -- ^ how to parse a head
  -> (Guard m -> m el) -- ^ callback to parse a body
  -> m a -- callback's result transformed by the result of head parser
headedOptional pHead pEl = headedBlock do
  h <- pHead
  pure $ BodyOpt (h Nothing) (fmap (h . Just) . pEl)
{-# INLINEABLE headedOptional #-}

headedSome :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => m ([el] -> a) -- ^ how to parse a head
  -> m el -- ^ how to parse each element of the body
  -> m a -- result of the head parser, applied to parsed elements of body
headedSome pHead pEl = headedBlock $
  fmap (<$> someBody pEl) pHead
{-# INLINEABLE headedSome #-}

headedMany :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
  => m ([el] -> a) -- ^ how to parse a head
  -> m el -- ^ how to parse each element of the body
  -> m a -- result of the head parser, applied to parsed elements of body
headedMany pHead pEl = headedBlock $
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

-- | general version of lineFold. Is not exposed
lineFoldWithG :: (TraversableStream s, MonadParsec e s m, MonadParsec e s n) =>
  Ordering -> Pos -> n () -> Scn n -> (n () -> m a) -> m a
lineFoldWithG ord ref sc scn action =
  replaceLineFoldError . action . void $ sc *> (optional . try) do
    st <- getParserState
    lvl' <- unScn scn *> L.indentLevel
    unless (lvl' `compare` ref == ord) do
      o <- getOffset
      setParserState st
      let i = LineFoldErrInfo o ord ref lvl'
      failure Nothing (E.singleton $ Label $ NE.fromList $ show i)
{-# INLINEABLE lineFoldWithG #-}
