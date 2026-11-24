{
module Examples.Mini.Parser (parseProgram) where

import           Examples.Mini.Lexer
}

%name parseProgram Program
%error {parseError}
%lexer {lexwrap} {L _ Eof _}
%monad {Alex}
%tokentype {Lexeme}
%token
    if			{ L _ KwIf			_ }
    print		{ L _ KwPrint			_ }
    while		{ L _ KwWhile			_ }
    ID_VAR		{ L _ IdVar			_ }
    LIT_INT		{ L _ LitInt			_ }
    '=='		{ L _ PctEqEq			_ }
    '='			{ L _ PctEq			_ }
    '+'			{ L _ PctPlus			_ }
    '('			{ L _ PctLParen			_ }
    ')'			{ L _ PctRParen			_ }
    ';'			{ L _ PctSemicolon		_ }
    '{'			{ L _ PctLBrace			_ }
    '}'			{ L _ PctRBrace			_ }
    '#define'		{ L _ PpDefine			_ }
    '\n'		{ L _ PpNewline			_ }
    '/*'		{ L _ CmtStart			_ }
    CMT_WORD		{ L _ CmtWord			_ }
    CMT_SPACE		{ L _ CmtSpace			_ }
    '*/'		{ L _ CmtEnd			_ }

%%

Program :: { () }
Program
:	Stmts						{ () }

Stmts :: { () }
Stmts
:							{ () }
|	Stmts Stmt					{ () }

Stmt :: { () }
Stmt
:	ID_VAR '=' Expr ';'				{ () }
|	print Expr ';'					{ () }
|	if '(' Expr ')' '{' Stmts '}'			{ () }
|	while '(' Expr ')' '{' Stmts '}'		{ () }
|	'#define' ID_VAR Expr '\n'			{ () }
|	'/*' CmtWords '*/'				{ () }

Expr :: { () }
Expr
:	Term						{ () }
|	Expr '+' Term					{ () }
|	Expr '==' Term					{ () }

Term :: { () }
Term
:	ID_VAR						{ () }
|	LIT_INT						{ () }
|	'(' Expr ')'					{ () }

CmtWords :: { () }
CmtWords
:	CMT_WORD					{ () }
|	CmtWords CMT_SPACE CMT_WORD			{ () }

{
lexwrap :: (Lexeme -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Lexeme -> Alex a
parseError (L _ c s) = alexError $ "parse error at " ++ show c ++ " " ++ show s
}
