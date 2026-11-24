{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Trees made from a grammar: rules with their symbols, down to tokens.
module Language.Happy.GenTree
    ( GenTree (..)
    , flatten
    , renderTree
    , getRules
    ) where

import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Language.Happy.Ast (Sym (..))

-- | A rule with the trees of its symbols, or a token.
data GenTree token
    = Node Sym [GenTree token]
    | Leaf token
    deriving (Show, Eq, Functor, Generic)

-- | Returns the tokens of the tree, in order.
flatten :: GenTree token -> [token]
flatten (Leaf t)    = [t]
flatten (Node _ cs) = concatMap flatten cs

-- | Returns the names of the rules in the tree, once for every use.
getRules :: GenTree token -> [Text]
getRules (Leaf _)               = []
getRules (Node (Sym name _) cs) = name : concatMap getRules cs

-- | Shows the tree as indented lines, for debugging.
renderTree :: Show token => GenTree token -> String
renderTree = go 0
  where
    go indent (Leaf t) = replicate indent ' ' ++ show t ++ "\n"
    go indent (Node sym children) =
        replicate indent ' ' ++ show sym ++ "\n" ++ concatMap (go (indent + 2)) children
