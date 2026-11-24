-- | Parses Happy @.y@ files: @runAlex text parseGrammar@.
module Language.Happy (module X, Grammar) where

import           Data.Text             (Text)
import           Language.Happy.Ast    as X
import           Language.Happy.Lexer  as X
import           Language.Happy.Parser as X

-- | A parsed Happy file.
type Grammar = Node (Lexeme Text)
