{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Text.Megaparsec.Char.Lexer.Stateful (
    -- * Space consumer wrappers
    C.Scn,
    -- * Space consumer's transformer
    ScT,
    runScT,
    mapScT,
    -- * Space consumer's monad
    MonadParsecSc (..),
    setLocalSc,
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
    nonIndented,
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

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.RWS (MonadReader(..), MonadState)
import Control.Monad.Reader (MonadTrans(..), ReaderT(..), mapReaderT)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S
import Data.CaseInsensitive (FoldCase)
import Data.Kind (Type)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer.New.Common (Scn)
import qualified Text.Megaparsec.Char.Lexer.New.Common as C

newtype ScT m a
  = ScT { unScT :: ReaderT (m ()) m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus)

runScT :: ScT m a -> m () -> m a
runScT = runReaderT . unScT

mapScT :: (m a -> m b) -> ScT m a -> ScT m b
mapScT f = ScT . mapReaderT f . unScT

instance MonadTrans ScT where
  lift = ScT . lift

deriving instance (MonadParsec e s m) => MonadParsec e s (ScT m)
deriving instance (MonadState s m) => MonadState s (ScT m)
deriving instance (MonadError e m) => MonadError e (ScT m)
instance MonadReader r m => MonadReader r (ScT m) where
  reader = lift . reader
  ask = lift ask
  local f = mapScT (local f)

class (MonadParsec e s m, MonadParsec e s (BaseSc m)) => MonadParsecSc e s m where
  type BaseSc m :: Type -> Type
  askSc :: m (BaseSc m ())
  default askSc :: (m ~ t n, MonadTrans t, MonadParsecSc e s n, BaseSc n ~ BaseSc m)
    => m (BaseSc m ())
  askSc = lift askSc
  sc :: m ()
  default sc :: (m ~ t n, MonadTrans t, MonadParsecSc e s n) => m ()
  sc = lift sc
  localSc :: (BaseSc m () -> BaseSc m ()) -> m a -> m a

setLocalSc :: MonadParsecSc e s m => BaseSc m () -> m a -> m a
setLocalSc = localSc . const
{-# INLINE setLocalSc #-}

instance MonadParsec e s m => MonadParsecSc e s (ScT m) where
  type BaseSc (ScT m) = m
  askSc = ScT ask
  sc = ScT $ ask >>= lift
  localSc f = ScT . local f . unScT
instance MonadParsecSc e s m => MonadParsecSc e s (ReaderT r m) where
  type BaseSc (ReaderT r m) = BaseSc m
  localSc f = mapReaderT $ localSc f
instance MonadParsecSc e s m => MonadParsecSc e s (L.StateT s' m) where
  type BaseSc (L.StateT s' m) = BaseSc m
  localSc f = L.mapStateT $ localSc f
instance MonadParsecSc e s m => MonadParsecSc e s (S.StateT s' m) where
  type BaseSc (S.StateT s' m) = BaseSc m
  localSc f = S.mapStateT $ localSc f
instance (Monoid w, MonadParsecSc e s m) => MonadParsecSc e s (L.WriterT w m) where
  type BaseSc (L.WriterT w m) = BaseSc m
  localSc f = L.mapWriterT $ localSc f
instance (Monoid w, MonadParsecSc e s m) => MonadParsecSc e s (S.WriterT w m) where
  type BaseSc (S.WriterT w m) = BaseSc m
  localSc f = S.mapWriterT $ localSc f

lexeme :: MonadParsecSc e s m => m a -> m a
lexeme = L.lexeme sc
{-# INLINE lexeme #-}

symbol :: MonadParsecSc e s m => Tokens s -> m (Tokens s)
symbol = L.symbol sc
{-# INLINE symbol #-}

symbol' :: (MonadParsecSc e s m, FoldCase (Tokens s)) =>
  Tokens s -> m (Tokens s)
symbol' = L.symbol' sc
{-# INLINE symbol' #-}

-- | Parse a non-indented construction. This ensures that there is no
-- indentation before actual data. Useful, for example, as a wrapper for
-- top-level function definitions.
nonIndented :: (TraversableStream s, MonadParsecSc e s m) =>
  m a -> m a
nonIndented = L.nonIndented sc
{-# INLINEABLE nonIndented #-}

lineFoldWith :: (TraversableStream s, MonadParsecSc e s m) =>
  Ordering -> Pos -> Scn (BaseSc m) -> m a -> m a
lineFoldWith ord ref scn p = askSc >>= \sc0 ->
  C.lineFoldWith ord ref sc0 scn (`setLocalSc` p)
{-# INLINEABLE lineFoldWith #-}

lineFold :: (TraversableStream s, MonadParsecSc e s m) =>
  Scn (BaseSc m) -> m a -> m a
lineFold scn p = L.indentLevel >>= \lvl -> lineFoldWith GT lvl scn p
{-# INLINEABLE lineFold #-}

paragraph :: (TraversableStream s, MonadParsecSc e s m) =>
  Scn (BaseSc m) -> m a -> m a
paragraph scn p = L.indentLevel >>= \lvl -> lineFoldWith EQ lvl scn p
{-# INLINEABLE paragraph #-}
