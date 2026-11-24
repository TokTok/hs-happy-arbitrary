{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | Random trees and token lists for a Happy grammar.
module Language.Happy.Arbitrary
    ( Config (..)
    , defConfig
    , genTokens
    , genTree
    ) where

import           Data.Text              (Text)
import           Language.Happy.Ast     (Node, Sym (..))
import           Language.Happy.GenTree (GenTree, flatten)
import           Language.Happy.Grammar (extractTokens, fromAst)
import           Language.Happy.Lexer   (Lexeme)
import           Language.Happy.Sized   (genSized)
import qualified Test.QuickCheck.Gen    as Gen
import           Test.QuickCheck.Gen    (Gen)

-- | How to make a token from the code of its @%token@ pattern.
newtype Config token = Config
    { parseToken :: Text -> token
    }

defConfig :: (Text -> token) -> Config token
defConfig parseToken = Config{parseToken}

-- | Returns the tokens of a tree from 'genTree'.
genTokens :: Config token -> Text -> Node (Lexeme Text) -> Gen [token]
genTokens cfg start g = flatten <$> genTree cfg start g

-- | Draws trees with QuickCheck's size in tokens, or as close as the grammar
-- allows.
genTree :: Config token -> Text -> Node (Lexeme Text) -> Gen (GenTree token)
genTree cfg start g = Gen.sized gen
  where
    -- Shared between draws, so the size ranges are worked out once.
    gen = genSized (fromAst start g) (extractTokens (parseToken cfg) g) (Sym start [])
