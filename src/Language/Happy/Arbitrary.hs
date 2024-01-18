{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Happy.Arbitrary where

import           Control.Applicative       ((<|>))
import           Control.Monad.Extra       (concatMapM)
import           Control.Monad.State.Lazy  (State)
import qualified Control.Monad.State.Lazy  as State
import           Data.Fix                  (foldFix)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Debug.Trace               (traceM)
import           Language.Happy.Ast        (Node, NodeF (..))
import           Language.Happy.Lexer      (Lexeme, lexemeText)
import           Test.QuickCheck.Arbitrary (arbitrary)
import qualified Test.QuickCheck.Gen       as Gen
import           Test.QuickCheck.Gen       (Gen)

newtype Config token = Config
    { parseToken :: Text -> token
    }

defConfig :: (Text -> token) -> Config token
defConfig parseToken = Config{parseToken}

genTokens :: Show token => Config token -> Text -> Node (Lexeme Text) -> Gen [token]
genTokens cfg start g = do
    rng <- Gen.scale (*2) arbitrary
    -- _ <- traceM $ "============================\nrng = " <> show rng
    return $ case Map.lookup start rules of
        Nothing -> error $ "no such rule: " <> Text.unpack start
        Just r  -> State.evalState (expand cfg tokens rules r) rng
  where
    tokens = foldFix (terminals cfg) g
    rules = foldFix nonterminals g

expand :: Show token => Config token -> Map Text token -> Map Text [[Text]] -> [[Text]] -> State [Int] [token]
expand cfg tokens rules nonterm = do
    -- _ <- traceM $ show rng <> ": selecting from " <> show nonterm
    rule <- select nonterm
    res <- concatMapM (continue cfg tokens rules . resolve tokens rules) rule
    -- _ <- traceM $ show rng <> ": result: " <> show res
    return res

select :: [a] -> State [Int] a
select [] = error "nope"
select nonterm@(rule:_) = do
    rng <- State.get
    case rng of
        [] -> return rule
        (i:is) -> do
            State.put is
            return $ nonterm !! (i `mod` length nonterm)

continue :: Show token => Config token -> Map Text token -> Map Text [[Text]] -> Either token [[Text]] -> State [Int] [token]
continue _ _ _ (Left token)            = return [token]
continue cfg tokens rules (Right rule) = expand cfg tokens rules rule

resolve :: Show token => Map Text token -> Map Text [[Text]] -> Text -> Either token [[Text]]
resolve tokens rules sym =
    fromJust $ (Left <$> Map.lookup sym tokens) <|> (Right <$> Map.lookup sym rules)

terminals :: Config token -> NodeF (Lexeme Text) (Map Text token) -> Map Text token
terminals Config{parseToken} node = case node of
    Token k v -> Map.singleton (lexemeText k) (parseToken $ lexemeText v)
    n         -> Map.unions n

nonterminals :: NodeF (Lexeme Text) (Map Text [[Text]]) -> Map Text [[Text]]
nonterminals node = case node of
    RuleLine syms _     -> Map.singleton "" [map lexemeText syms]
    RuleDefn name rules -> Map.singleton (lexemeText name) (merge rules)
    n                   -> Map.unionsWith (++) n
  where
    merge = concat . concatMap Map.elems
