{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict            #-}
module Language.Happy.Ast
    ( Node, NodeF (..)
    ) where

import           Data.Aeson                   (FromJSON, FromJSON1, ToJSON,
                                               ToJSON1)
import           Data.Fix                     (Fix)
import           Data.Functor.Classes         (Eq1, Ord1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           GHC.Generics                 (Generic, Generic1)

data NodeF lexeme a
    = Grammar [lexeme] [a] [a] [lexeme]
    | PragmaExpect lexeme
    | PragmaName lexeme lexeme
    | PragmaErrorHandlerType lexeme
    | PragmaError lexeme
    | PragmaLexer lexeme lexeme
    | PragmaMonad lexeme
    | PragmaTokenType lexeme
    | PragmaToken [a]
    | PragmaLeft [lexeme]
    | PragmaRight [lexeme]
    | Token lexeme lexeme
    | Rule a a
    | RuleType lexeme lexeme
    | RuleDefn lexeme [a]
    | RuleLine [lexeme] lexeme
    deriving (Show, Read, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)
    deriving (Show1, Read1, Eq1, Ord1) via FunctorClassesDefault (NodeF lexeme)

type Node lexeme = Fix (NodeF lexeme)

instance FromJSON lexeme => FromJSON1 (NodeF lexeme)
instance ToJSON lexeme => ToJSON1 (NodeF lexeme)
