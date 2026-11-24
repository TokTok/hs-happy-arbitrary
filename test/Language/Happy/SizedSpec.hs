{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.SizedSpec (spec) where

import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Language.Happy.Ast     (Sym (..))
import           Language.Happy.GenTree (flatten)
import           Language.Happy.Grammar (Rules)
import           Language.Happy.Sized   (Range (..), genSized, ranges)
import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Test.QuickCheck        (choose, forAll, (===))

sym :: Text -> Sym
sym name = Sym name []

-- | @Pair : A A@, @A : x | x y@, @List : | List x@, @Gap : x | x x x@.
rules :: Rules
rules = Map.fromList
    [ (sym "Pair", [[sym "A", sym "A"]])
    , (sym "A", [[sym "x"], [sym "x", sym "y"]])
    , (sym "List", [[], [sym "List", sym "x"]])
    , (sym "Gap", [[sym "x"], [sym "x", sym "x", sym "x"]])
    , (sym "Never", [[sym "Never", sym "x"]])
    ]

tokens :: Map.Map Text Text
tokens = Map.fromList [("x", "x"), ("y", "y")]

spec :: Spec
spec = do
    describe "ranges" $
        it "finds the fewest and the most tokens, and leaves out what makes no tree" $
            ranges rules (Map.keysSet tokens) `shouldBe` Map.fromList
                [ (sym "Pair", Range 2 (Just 4))
                , (sym "A", Range 1 (Just 2))
                , (sym "List", Range 0 Nothing)
                , (sym "Gap", Range 1 (Just 3))
                ]

    describe "genSized" $ do
        it "makes trees of exactly the size asked for" $
            forAll (choose (0, 300)) $ \n ->
                forAll (genSized rules tokens (sym "List") n) $ \tree ->
                    length (flatten tree) === n

        it "makes the closest size when the asked one is impossible" $
            forAll (genSized rules tokens (sym "Gap") 2) $ \tree ->
                length (flatten tree) `elem` [1, 3]
