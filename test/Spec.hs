{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Control.Monad (void)
import Data.Foldable (for_)
import Data.Void (Void)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer.New as LN
import qualified Text.Megaparsec.Char.Lexer.Stateful as LS

type Parser = Parsec Void String
type ParserM = MonadParsec Void String

sc :: ParserM m => LN.Sc m
scn :: ParserM m => LN.Scn m
scn1 :: ParserM m => LN.Scn1 m
(sc, scn, scn1) = LN.makeScs hspace1 (void eol) empty empty

line :: MonadParsec Void String m => m a -> m a
line = LN.line scn1

main :: IO ()
main = hspec do
  describe "block simple" do
    let pn = LN.block \grd -> do
            a <- line $ LN.symbol sc "a"
            grd
            b <- line $ LN.symbol sc "b"
            grd
            c <- line $ LN.symbol sc "c"
            pure [a, b, c]
        ps = flip LS.runScT sc $ LS.block \grd -> do
            a <- line $ LS.symbol "a"
            grd
            b <- line $ LS.symbol "b"
            grd
            c <- line $ LS.symbol "c"
            pure [a, b, c]
    for_ [(pn, "New"), (ps, "Stateful")] \(p, lbl) -> describe lbl do
      it "simple" do
        prs p "a\nb\nc" `shouldParse` ["a", "b", "c"]
        prs' p "a\nb\nc" `succeedsLeaving` ""
      it "extra spaces" do
        prs p "a  \nb \nc   \n  " `shouldParse` ["a", "b", "c"]
        prs' p "a  \nb \nc   \n  " `succeedsLeaving` ""
      it "indented (1)" do
        prs p " a\n b\n c\n" `shouldParse` ["a", "b", "c"]
        prs' p " a\n b\n c\n" `succeedsLeaving` ""
      it "incorrect indentation (inc)" do
        prs' p "a\n b\nc\n" `failsLeaving` "b\nc\n"
      it "incorrect indentation (dec)" do
        prs' p " a\nb\nc\n" `failsLeaving` "b\nc\n"
      it "incorrect indentation II (inc)" do
        prs' p "a\nb\n c\n" `failsLeaving` "c\n"
      it "incorrect indentation II (dec)" do
        prs' p " a\n b\nc\n" `failsLeaving` "c\n"
  describe "headedMany" do
    let pn = (length <$ LN.mayLine scn (LN.symbol sc "a"))
                `LN.headedMany` line (LN.symbol sc "a")
        ps = flip LS.runScT sc $
          (length <$ LS.mayLine scn (LS.symbol "a"))
            `LS.headedMany` line (LS.symbol "a")
    for_ [(pn, "New"), (ps, "Stateful")] \(p, lbl) -> describe lbl do
      it "empty" do
        prs p "a" `shouldParse` 0
        prs' p "a" `succeedsLeaving` ""
      it "empty with eol" do
        prs p "a\n" `shouldParse` 0
        prs' p "a\n" `succeedsLeaving` ""
      it "empty with eols" do
        prs p "a  \n  \n \n " `shouldParse` 0
        prs' p "a  \n  \n \n" `succeedsLeaving` ""
      it "empty with eols and white space" do
        prs p "a  \n  \n \n " `shouldParse` 0
        prs' p "a  \n  \n \n " `succeedsLeaving` ""
      it "one" do
        prs p "a\n a" `shouldParse` 1
        prs' p "a\n a" `succeedsLeaving` ""
      it "one with eol" do
        prs p "a\n a\n" `shouldParse` 1
        prs' p "a\n a\n" `succeedsLeaving` ""
      it "one with eol and white space" do
        prs p "a\n a\n" `shouldParse` 1
        prs' p "a\n a\n " `succeedsLeaving` ""

  describe "block > headedOne" do
    let pn = LN.block \grd -> do
            as <- LN.mayLine scn ((:) <$> LN.symbol sc "a")
              `LN.headedOne` \grd' -> do
                a' <- line $ LN.symbol sc "a'"
                grd'
                a'' <- line $ LN.symbol sc "a''"
                pure [a', a'']
            grd
            b <- line $ LN.symbol sc "b"
            grd
            c <- line $ LN.symbol sc "c"
            pure $ as <> [b, c]
        ps = flip LS.runScT sc $ LS.block \grd -> do
            as <- LN.mayLine scn ((:) <$> LS.symbol "a")
              `LS.headedOne` \grd' -> do
                a' <- line $ LS.symbol "a'"
                grd'
                a'' <- line $ LS.symbol "a''"
                pure [a', a'']
            grd
            b <- line $ LS.symbol "b"
            grd
            c <- line $ LS.symbol "c"
            pure $ as <> [b, c]
    for_ [(pn, "New"), (ps, "Stateful")] \(p, lbl) -> describe lbl do
      it "simple" do
        prs p "a\n a'\n a''\nb\nc" `shouldParse` ["a", "a'", "a''", "b", "c"]
        prs' p "a\n a'\n a''\nb\nc" `succeedsLeaving` ""
      it "extra spaces" do
        prs p "a   \n  a' \n  a''\nb  \nc   \n "
          `shouldParse` ["a", "a'", "a''", "b", "c"]
        prs' p "a   \n  a' \n  a''\nb  \nc   \n "
          `succeedsLeaving` ""
  -- describe "block > headedMany" do
  --   let nHeadedMany = LN.headedMany space space
  --       sHeadedMany = LS.headedMany space space
  --       pn = LN.block space \sp -> do
  --           as <- ((:) <$> LN.symbol sc "a")
  --             `nHeadedMany` LN.symbol sc "a'"
  --           sp
  --           b <- LN.symbol sc "b"
  --           sp
  --           c <- LN.symbol sc "c"
  --           pure $ as <> [b, c]
  --       ps = flip LS.runScT hspace $ LS.block space \sp -> do
  --           as <- ((:) <$> LS.symbol "a")
  --             `sHeadedMany` LS.symbol "a'"
  --           sp
  --           b <- LS.symbol "b"
  --           sp
  --           c <- LS.symbol "c"
  --           pure $ as <> [b, c]
  --   for_ [(pn, "New"), (ps, "Stateful")] \(p, lbl) -> describe lbl do
  --     it "simple" do
  --       prs p "a\n a'\n a'\nb\nc" `shouldParse` ["a", "a'", "a'", "b", "c"]
  --       prs' p "a\n a'\n a'\nb\nc" `succeedsLeaving` ""
  --     it "extra spaces" do
  --       prs p "a   \n  a' \n  a'\nb  \nc   \n "
  --         `shouldParse` ["a", "a'", "a'", "b", "c"]
  --       prs' p "a   \n  a' \n  a'\nb  \nc   \n "
  --         `succeedsLeaving` "\n "

-- shouldParse' ::
--   (HasCallStack, ShowErrorComponent e, Stream s, VisualStream s,
--   TraversableStream s, Show a, Eq a) =>
--   Either (ParseErrorBundle s e) a -> a -> Expectation
-- shouldParse' = shouldParse

-- succeedsLeaving' ::
--   (HasCallStack, Show a, Eq s, Show s, ShowErrorComponent e,
--   Stream s, VisualStream s, TraversableStream s) =>
--   (State s e, Either (ParseErrorBundle s e) a) -> s -> Expectation
-- succeedsLeaving' = succeedsLeaving


prs :: Parser a -> String -> Either (ParseErrorBundle String Void) a
prs p = runParser (space *> p) ""

prs' :: Parser a -> String ->
  (State String Void, Either (ParseErrorBundle String Void) a)
prs' p str = runParser' (space *> p) (initialState str)
