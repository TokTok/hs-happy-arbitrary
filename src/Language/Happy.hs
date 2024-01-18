module Language.Happy (module X, Grammar) where

import           Data.Text             (Text)
import           Language.Happy.Ast    as X
import           Language.Happy.Lexer  as X
import           Language.Happy.Parser as X


type Grammar = Node (Lexeme Text)
