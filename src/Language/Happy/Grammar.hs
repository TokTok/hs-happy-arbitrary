{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

-- | The rules of a Happy grammar, with parameterised rules like @List(p)@
-- instantiated for every argument they're used with.
module Language.Happy.Grammar
    ( Production
    , Rules
    , Sym (..)
    , extractTokens
    , fromAst
    ) where

import           Control.Monad.State  (State, execState, gets, modify)
import           Data.Fix             (Fix (..))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Language.Happy.Ast   (Node, NodeF (..), RuleDef (..), Sym (..))
import           Language.Happy.Lexer (Lexeme, lexemeText)

-- | The symbols on the right of a rule, in order.
type Production = [Sym]

-- | The productions of every nonterminal that the start symbol reaches.
type Rules = Map Sym [Production]

-- | Returns the rules of a parsed Happy file that the start rule reaches.
fromAst :: Text -> Node (Lexeme Text) -> Rules
fromAst start node = expandGrammar (extractRules node) (extractTokenNames node) (Sym start [])

-- Extraction Logic

extractRules :: Node (Lexeme Text) -> Map Text [RuleDef]
extractRules (Fix (Grammar _ _ rules _)) = Map.unionsWith (++) (map extractRule rules)
extractRules _ = mempty

extractRule :: Node (Lexeme Text) -> Map Text [RuleDef]
extractRule (Fix (RuleDefn name params ruleLines)) =
    let paramNames = map lexemeText params
        prods = extractRuleLines ruleLines
    in Map.singleton (lexemeText name) [RuleDef paramNames prods]
extractRule (Fix (RuleType _ _)) = mempty
extractRule node = error $ "Unexpected node in Rules list: " ++ show node

extractRuleLines :: [Node (Lexeme Text)] -> [[Sym]]
extractRuleLines = map extractRuleLine

extractRuleLine :: Node (Lexeme Text) -> [Sym]
extractRuleLine (Fix (RuleLine syms _)) = map extractSymbol syms
extractRuleLine _                       = []

extractSymbol :: Node (Lexeme Text) -> Sym
extractSymbol (Fix (Symbol name args)) = Sym (lexemeText name) (map extractSymbol args)
extractSymbol _ = error "Invalid AST: Expected Symbol"

extractTokenNames :: Node (Lexeme Text) -> Set Text
extractTokenNames (Fix (Grammar _ pragmas _ _)) =
    Set.unions (map extractPragmaTokenNames pragmas)
extractTokenNames _ = mempty

extractPragmaTokenNames :: Node (Lexeme Text) -> Set Text
extractPragmaTokenNames (Fix (PragmaToken tokens)) =
    Set.fromList [ lexemeText name | Fix (Token name _) <- tokens ]
extractPragmaTokenNames _ = mempty

-- | Makes every token from the code of its @%token@ pattern.
extractTokens :: (Text -> token) -> Node (Lexeme Text) -> Map Text token
extractTokens parseFunc (Fix (Grammar _ pragmas _ _)) =
    Map.unions (map (extractPragmaToken parseFunc) pragmas)
extractTokens _ _ = mempty

extractPragmaToken :: (Text -> token) -> Node (Lexeme Text) -> Map Text token
extractPragmaToken parseFunc (Fix (PragmaToken tokens)) =
    Map.fromList [ (lexemeText name, parseFunc (lexemeText code))
                 | Fix (Token name code) <- tokens ]
extractPragmaToken _ _ = mempty

-- Expansion Logic

-- | Symbols that are neither tokens nor rules, like Happy's @error@, are left
-- in the productions. They make no trees, so the generator leaves out the
-- productions that have them.
expandGrammar :: Map Text [RuleDef] -> Set Text -> Sym -> Rules
expandGrammar rules tokens start = execState (process [start]) Map.empty
  where
    process :: [Sym] -> State Rules ()
    process [] = return ()
    process (sym@(Sym name args) : rest) = do
        visited <- gets (Map.member sym)
        case Map.lookup name rules of
            Just defs | not visited && not (Set.member name tokens) -> do
                let prods = [ map (substitute (bind params)) prod | RuleDef params alts <- defs, prod <- alts ]
                    bind params
                        | length params == length args = Map.fromList (zip params args)
                        | otherwise = error $ "fromAst: " ++ show sym ++ " has the wrong number of arguments"
                modify (Map.insert sym prods)
                process (concat prods ++ rest)
            _ -> process rest

    substitute :: Map Text Sym -> Sym -> Sym
    substitute env (Sym n a) = case Map.lookup n env of
        Just arg | null a -> arg
                 | otherwise -> error $ "fromAst: rule parameter " ++ show n ++ " can't take arguments"
        Nothing -> Sym n (map (substitute env) a)
