{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
module Language.Happy.Tokens
    ( LexemeClass (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

-- | The kinds of tokens in a Happy file.
data LexemeClass
    = IdName

    | KwErrorhandlertype
    | KwError
    | KwExpect
    | KwLeft
    | KwLexer
    | KwMonad
    | KwName
    | KwNonassoc
    | KwPartial
    | KwPrec
    | KwRight
    | KwToken
    | KwTokentype

    | PctColon
    | PctColonColon
    | PctComma
    | PctLParen
    | PctPercentPercent
    | PctPipe
    | PctRParen

    | LitCode
    | LitInteger
    | LitString

    | ErrorToken
    | Eof
    deriving (Enum, Bounded, Ord, Eq, Show, Read, Generic)

instance FromJSON LexemeClass
instance ToJSON LexemeClass
