{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion (Benchmark, bench, bgroup, whnf)
import Criterion.Main (defaultMain)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer.New (Sc(..))
import qualified Text.Megaparsec.Char.Lexer.New as LN
import qualified Text.Megaparsec.Char.Lexer.Stateful as LS

type Parser = Parsec Void Text
type ParserM = MonadParsec Void Text

ns :: [Int]
ns = [500, 1000, 2000, 4000]

sc, scn, scn1, scn11 :: Parser ()
sc = hspace
-- | Consumes line space and eols, possibly nothing
scn = hidden space
-- | Consumes line space and eols, including at least one eol
scn1 = label "end of line" $ hspace *> eol *> hidden space
-- | Consumes line space and exactly one eol
scn11 = label "end of line" $ hspace *> eol *> hidden hspace

main :: IO ()
main = defaultMain [
    bgroup "lineFold" $
      let p  = some $ do
                res <- LN.lineFold (Sc sc) scn $ \sc' ->
                  some $ LN.symbol sc' "a"
                scn
                pure res
          p1 = (`LS.runScT` sc) $ some $ do
                res <- LS.lineFold space $
                  some $ LS.symbol "a"
                space
                pure res
          p0 = some $ do
                res <- L.lineFold space $ \sc' ->
                  string "a" `sepBy1` (try sc' <* notFollowedBy eof)
                scn
                pure res
      in
      [ bgroup "only line spaces" $
          let gen n = T.replicate n "a "
           in compParsers gen p0 p p1
      , bgroup "multiple lines fold with the same indent" $
          let gen n = "a\n" <> T.replicate (n - 1) " a\n"
           in compParsers gen p0 p p1
      , bgroup "multiple lines without folds" $
          let gen n = "a\n" <> T.replicate (n - 1) "a\n"
           in compParsers gen p0 p p1
      , bgroup "known line fold end" $
          let gen n = T.replicate (n `div` 3) "a\n b\n c\n"
              q0 = some $ do
                    res <- L.lineFold scn $ \sc' -> do
                      a <- L.symbol sc' "a"
                      b <- L.symbol sc' "b"
                      c <- L.symbol sc  "c"
                      pure [a, b, c]
                    scn
                    pure res
              q = some $ do
                    res <- LN.lineFold (Sc sc) scn $ \sc' -> do
                      a <- LN.symbol sc' "a"
                      b <- LN.symbol sc' "b"
                      c <- LN.symbol sc' "c"
                      pure [a, b, c]
                    scn
                    pure res
              q1 = (`LS.runScT` sc) $ some do
                    res <- LS.lineFold scn $ do
                      a <- LS.symbol "a"
                      b <- LS.symbol "b"
                      c <- LS.symbol "c"
                      pure [a, b, c]
                    space
                    pure res
           in compParsers gen q0 q q1
      ]
    ]

compParsers :: (Int -> Text) -> Parser b -> Parser b -> Parser b -> [Benchmark]
compParsers gen p0 p p1 =
    [ bgroup "old" $ bParser gen p0 <$> ns
    , bgroup "new" $ bParser gen p  <$> ns
    , bgroup "stateful" $ bParser gen p1  <$> ns
    ]

bParser :: (Int -> Text) -> Parser b -> Int ->  Benchmark
bParser gen p = bGen gen (runParser p "")

bGen :: (Int -> a) -> (a -> b) -> Int ->  Benchmark
bGen gen f n = bench (show n) $ whnf f (gen n)
