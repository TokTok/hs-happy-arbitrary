{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Happy.Arbitrary where

import           Control.Applicative  ((<|>))
import           Control.Monad.Extra  (concatMapM)
import           Data.Fix             (foldFix)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Language.Happy.Ast   (Node, NodeF (..))
import           Language.Happy.Lexer (Lexeme, lexemeText)
import qualified Test.QuickCheck.Gen  as Gen
import           Test.QuickCheck.Gen  (Gen)

genTokens :: Text -> Node (Lexeme Text) -> Gen [Text]
genTokens start g = do
    case Map.lookup start rules of
        Nothing -> error $ "no such rule: " <> Text.unpack start
        Just r  -> expand tokens rules r
  where
    tokens = foldFix terminals g
    rules = foldFix nonterminals g

expand :: Map Text Text -> Map Text [[Text]] -> [[Text]] -> Gen [Text]
expand tokens rules r = do
    rule <- Gen.elements r
    let expanded = map (resolve tokens rules) rule
    concatMapM (continue tokens rules) expanded

continue :: Map Text Text -> Map Text [[Text]] -> Either Text [[Text]] -> Gen [Text]
continue _ _ (Left token)          = return [token]
continue tokens rules (Right rule) = expand tokens rules rule

resolve :: Map Text Text -> Map Text [[Text]] -> Text -> Either Text [[Text]]
resolve tokens rules sym =
    fromJust $ (Left <$> Map.lookup sym tokens) <|> (Right <$> Map.lookup sym rules)

terminals :: NodeF (Lexeme Text) (Map Text Text) -> Map Text Text
terminals node = case node of
    Token k v -> Map.singleton (lexemeText k) (lexemeText v)
    n         -> Map.unions n

nonterminals :: NodeF (Lexeme Text) (Map Text [[Text]]) -> Map Text [[Text]]
nonterminals node = case node of
    RuleLine syms _     -> Map.singleton "" [map lexemeText syms]
    RuleDefn name rules -> Map.singleton (lexemeText name) (merge rules)
    n                   -> Map.unionsWith (++) n
  where
    merge = concat . concatMap Map.elems
