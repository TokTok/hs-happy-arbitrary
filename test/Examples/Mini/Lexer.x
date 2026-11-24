{
-- | A small language for testing the generator. Spaces matter in comments and
-- in @#define@ lines, keywords look like variable names, and @==@ looks like 2
-- @=@ without a space between them.
module Examples.Mini.Lexer
    ( Alex
    , Lexeme (..)
    , TokenClass (..)
    , alexError
    , alexMonadScan
    , lexClasses
    , runAlex
    ) where
}

%wrapper "monad"

tokens :-

<0>		"if"			{ mkL KwIf }
<0>		"print"			{ mkL KwPrint }
<0>		"while"			{ mkL KwWhile }
<0,ppSC>	[a-z][a-z0-9_]*		{ mkL IdVar }
<0,ppSC>	[0-9]+			{ mkL LitInt }
<0,ppSC>	"=="			{ mkL PctEqEq }
<0>		"="			{ mkL PctEq }
<0,ppSC>	"+"			{ mkL PctPlus }
<0,ppSC>	"("			{ mkL PctLParen }
<0,ppSC>	")"			{ mkL PctRParen }
<0>		";"			{ mkL PctSemicolon }
<0>		"{"			{ mkL PctLBrace }
<0>		"}"			{ mkL PctRBrace }
<0>		"#define"		{ mkL PpDefine `andBegin` ppSC }
<ppSC>		\n			{ mkL PpNewline `andBegin` 0 }
<ppSC>		" "+			;
<0>		"/*"			{ mkL CmtStart `andBegin` cmtSC }
<cmtSC>		[a-z]+			{ mkL CmtWord }
<cmtSC>		" "+			{ mkL CmtSpace }
<cmtSC>		"*/"			{ mkL CmtEnd `andBegin` 0 }
<0>		[\ \n]+			;

{
data TokenClass
    = KwIf
    | KwPrint
    | KwWhile
    | IdVar
    | LitInt
    | PctEqEq
    | PctEq
    | PctPlus
    | PctLParen
    | PctRParen
    | PctSemicolon
    | PctLBrace
    | PctRBrace
    | PpDefine
    | PpNewline
    | CmtStart
    | CmtWord
    | CmtSpace
    | CmtEnd
    | Eof
    deriving (Eq, Show)

data Lexeme = L AlexPosn TokenClass String
    deriving (Show)

mkL :: TokenClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, s) len = pure $ L p c (take len s)

alexEOF :: Alex Lexeme
alexEOF = pure $ L (AlexPn 0 0 0) Eof ""

lexClasses :: String -> Either String [TokenClass]
lexClasses s = runAlex s $ go []
  where
    go acc = do
        L _ c _ <- alexMonadScan
        if c == Eof then pure (reverse acc) else go (c : acc)
}
