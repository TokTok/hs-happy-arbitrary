{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict            #-}
-- | The syntax of a Happy @.y@ file.
module Language.Happy.Ast
    ( Node, NodeF (..)
    , Sym (..), RuleDef (..)
    ) where

import           Data.Aeson                   (FromJSON, FromJSON1, ToJSON,
                                               ToJSON1)
import           Data.Fix                     (Fix)
import           Data.Functor.Classes         (Eq1, Ord1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic, Generic1)

-- | A part of a Happy file, with its parts of type @a@.
data NodeF lexeme a
    = Grammar [lexeme] [a] [a] [lexeme]
    | PragmaExpect lexeme
    | PragmaName lexeme (Maybe lexeme)
    | PragmaPartial lexeme (Maybe lexeme)
    | PragmaErrorHandlerType lexeme
    | PragmaError lexeme
    | PragmaLexer lexeme lexeme
    | PragmaMonad [lexeme]
    | PragmaTokenType lexeme
    | PragmaToken [a]
    | PragmaLeft [lexeme]
    | PragmaRight [lexeme]
    | PragmaNonassoc [lexeme]
    | Token lexeme lexeme
    | RuleType lexeme lexeme
    | RuleDefn lexeme [lexeme] [a]
      -- ^ Name, parameters and productions.
    | RuleLine [a] lexeme
      -- ^ Symbols and code.
    | Symbol lexeme [a]
      -- ^ Name and arguments.
    deriving (Show, Read, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)
    deriving (Show1, Read1, Eq1, Ord1) via FunctorClassesDefault (NodeF lexeme)

-- | A Happy file, or a part of one.
type Node lexeme = Fix (NodeF lexeme)

instance FromJSON lexeme => FromJSON1 (NodeF lexeme)
instance ToJSON lexeme => ToJSON1 (NodeF lexeme)

-- | A use of a rule or token, with its arguments: @List(Identifier)@.
data Sym = Sym Text [Sym] deriving (Show, Eq, Ord)

-- | A rule's parameters and productions.
data RuleDef = RuleDef
    { ruleParams :: [Text]
    , ruleProds  :: [[Sym]]
    } deriving (Show, Eq)
