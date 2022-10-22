{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Criterion
import Criterion.Main
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
type ParserM = MonadParsec Void Text

pa :: ParserM m => m Text
pa = string "a"

ns :: [Int]
ns = (* 1000000) <$> [1, 2, 4]

main :: IO ()
main = defaultMain [] {-[
    bgroup "lineFold" $
      let p  = some $ do
                res <- L.lineFold hspace space $ \sc ->
                  some (pa <* sc)
                res <$ space
          p0 = some $ do
                res <- L.oldLineFold space $ \sc ->
                  pa `sepBy1` (try sc <* notFollowedBy eof)
                space
      in [
        bgroup "only line spaces" $
          let gen n = T.replicate n "a "
           in [ bgroup "old" $ benchPN gen p0 <$> ns
              , bgroup "new" $ benchPN gen p <$> ns
              ],
        bgroup "multiple lines fold with the same indent" $
          let gen n = "a\n" <> T.replicate (n - 1) " a\n"
           in [ bgroup "old" $ benchPN gen p0 <$> ns
              , bgroup "new" $ benchPN gen p <$> ns
              ],
        bgroup "multiple lines without folds" $
          let gen n = "a\n" <> T.replicate (n - 1) "a\n"
           in [ bgroup "old" $ benchPN gen p0 <$> ns
              , bgroup "new" $ benchPN gen p <$> ns
              ],
        bgroup "known line fold end" $
          let gen n = T.replicate (n `div` 3) "a\n b\n c\n"
              q = some $ do
                    res <- L.lineFold hspace space $ \sc -> do
                      a <- L.symbol sc "a"
                      b <- L.symbol sc "b"
                      c <- L.symbol sc "c"
                      pure [a, b, c]
                    space
                    pure res
              q0 = some $ do
                    res <- L.oldLineFold space $ \sc -> do
                      a <- L.symbol sc "a"
                      b <- L.symbol sc "b"
                      c <- L.symbol space "c"
                      pure [a, b, c]
                    pure res
           in [ bgroup "old" $ benchPN gen q0 <$> ns
              , bgroup "new" $ benchPN gen q <$> ns
              ]
      ]

    -- bgroup "local and imported lineFold" $
    --   let p  = some $ L.lineFold hspace space $ \sc -> some (pa *> sc)
    --       p' = some $ lineFold'  hspace space $ \sc -> some (pa *> sc)
    --   in [
    --     bgroup "only line spaces" $
    --       let gen n = T.replicate n "a "
    --        in [ bgroup "import" $ benchPN gen p <$> [n * 10000 | n <- [1, 2, 4]]
    --           , bgroup "local" $ benchPN gen p' <$> [n * 10000 | n <- [1, 2, 4]]
    --           ],
    --     bgroup "multiple lines fold with the same indent" $
    --       let gen n = "a\n" <> T.replicate (n - 1) " a\n"
    --        in [ bgroup "import" $ benchPN gen p <$> [n * 10000 | n <- [1, 2, 4]]
    --           , bgroup "local" $ benchPN gen p' <$> [n * 10000 | n <- [1, 2, 4]]
    --           ],
    --     bgroup "multiple lines without folds" $
    --       let gen n = "a\n" <> T.replicate (n - 1) " a\n"
    --        in [ bgroup "import" $ benchPN gen p <$> [n * 10000 | n <- [1, 2, 4]]
    --           , bgroup "local" $ benchPN gen p' <$> [n * 10000 | n <- [1, 2, 4]]
    --           ]
    --     ]
    ]
-}

benchPN :: (Int -> Text) -> Parser b -> Int ->  Benchmark
benchPN gen p = benchN gen (runParser p "")

benchN :: (Int -> a) -> (a -> b) -> Int ->  Benchmark
benchN gen f n = bench (show n) $ whnf f (gen n)
