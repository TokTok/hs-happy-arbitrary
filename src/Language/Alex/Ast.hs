{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
-- | An Alex lexer, as far as generating text for its tokens needs it. Macros
-- are expanded while parsing, so a regex stands on its own.
module Language.Alex.Ast
    ( AlexFile (..)
    , CharSet (..)
    , Regex (..)
    , Rule (..)
    ) where

import           Data.Set     (Set)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | A set of characters: these characters, or all characters but these.
data CharSet
    = Only (Set Char)
    | AllBut (Set Char)
    deriving (Show, Eq, Ord, Generic)

-- | A regex with its macros expanded.
data Regex
    = RString Text
    | RSet CharSet
    | RStar Regex
    | RPlus Regex
    | ROpt Regex
    | RSeq [Regex]
    | RAlt [Regex]
    deriving (Show, Eq, Ord, Generic)

-- | A lexer rule: where it applies, what it matches, and its action.
data Rule = Rule
    { ruleStartCodes :: [Text]
      -- ^ Empty means every start code.
    , ruleRegex      :: Regex
    , ruleCode       :: Text
      -- ^ The action, without braces, or @;@ for a rule that makes no token.
    }
    deriving (Show, Eq, Ord, Generic)

-- | The rules of a lexer, in order.
newtype AlexFile = AlexFile
    { alexRules :: [Rule]
    }
    deriving (Show, Eq, Ord, Generic)
