{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
module Language.Happy.Tokens
    ( LexemeClass (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data LexemeClass
    = IdName

    | KwErrorhandlertype
    | KwError
    | KwExpect
    | KwLeft
    | KwLexer
    | KwMonad
    | KwName
    | KwPrec
    | KwRight
    | KwToken
    | KwTokentype

    | PctColon
    | PctColonColon
    | PctPercentPercent
    | PctPipe

    | LitCode
    | LitInteger
    | LitString

    | ErrorToken
    | Eof
    deriving (Enum, Bounded, Ord, Eq, Show, Read, Generic)

instance FromJSON LexemeClass
instance ToJSON LexemeClass
