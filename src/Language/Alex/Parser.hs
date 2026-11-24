{-# LANGUAGE OverloadedStrings #-}
-- | Parses Alex @.x@ files. Macros are expanded as they're used. Left and
-- right contexts are parsed but not kept: they only restrict where a token
-- matches, and 'Language.Happy.Linker.renderChecked' asks the real lexer.
module Language.Alex.Parser
    ( parseAlex
    ) where

import           Control.Applicative  (many, optional, (<|>))
import           Data.Attoparsec.Text
import           Data.Char            (chr, isAlphaNum, isSpace)
import           Data.Functor         (($>))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Language.Alex.Ast
import           Numeric              (readOct)

-- | Parses an Alex file, or says why it can't.
parseAlex :: Text -> Either String AlexFile
parseAlex = parseOnly (alexFile <* endOfInput)

-- | The macros defined so far, by name without the sigil.
data Env = Env
    { envSets    :: Map Text CharSet
    , envRegexes :: Map Text Regex
    }

-- | Alex's own macros.
builtins :: Env
builtins = Env
    (Map.fromList
        [ ("white", Only (Set.fromList " \t\n\f\v\r"))
        , ("printable", AllBut (Set.fromList (['\0' .. '\31'] ++ ['\127'])))
        ])
    Map.empty

alexFile :: Parser AlexFile
alexFile = do
    spaces
    skipMany (codeBlock *> spaces)
    env <- definitions builtins
    _ <- identifier *> spaces *> string ":-" <?> "the start of the rules, like `tokens :-`"
    spaces
    rules <- concat <$> many (rule env <* spaces)
    skipMany (codeBlock *> spaces)
    return $ AlexFile rules

-- | Parses macros and directives like @%wrapper@, in any order.
definitions :: Env -> Parser Env
definitions env =
        (directive *> spaces *> definitions env)
    <|> (macro env >>= \env' -> spaces *> definitions env')
    <|> pure env

directive :: Parser ()
directive = char '%' *> identifier *> hspaces *> stringLiteral $> ()

macro :: Env -> Parser Env
macro env = setMacro <|> regexMacro
  where
    setMacro = do
        name <- char '$' *> identifier
        s <- spaces *> char '=' *> spaces *> charSet env
        return env{envSets = Map.insert name s (envSets env)}
    regexMacro = do
        name <- char '@' *> identifier
        r <- spaces *> char '=' *> spaces *> regex env
        return env{envRegexes = Map.insert name r (envRegexes env)}

-- | Parses a rule, or a block of rules for some start codes: @\<sc> { ... }@.
rule :: Env -> Parser [Rule]
rule env = do
    codes <- option [] (startCodes <* spaces)
    block codes <|> (pure <$> ruleBody codes)
  where
    block codes = char '{' *> spaces *> many (ruleBody codes <* spaces) <* char '}'

    ruleBody codes = do
        _ <- optional (char '^' <* hspaces)
        r <- regex env <?> "a regex"
        hspaces
        _ <- optional rightContext
        code <- spaces *> action <?> "an action"
        return $ Rule codes r code

    rightContext = (char '/' *> hspaces *> ((codeBlock $> ()) <|> (regex env $> ())) <* hspaces)
        <|> (char '$' *> hspaces)

    action = codeBlock <|> (char ';' $> ";")

startCodes :: Parser [Text]
startCodes = char '<' *> (identifier `sepBy1` char ',') <* char '>'

-- Regexes

regex :: Env -> Parser Regex
regex env = do
    alts <- sequence' `sepBy1` (spaces *> char '|' <* spaces)
    return $ case alts of
        [r] -> r
        rs  -> RAlt rs
  where
    sequence' = do
        terms <- many1 (term <* hspaces)
        return $ case terms of
            [r] -> r
            rs  -> RSeq rs

    term = do
        a <- atom
        postfixes a

    postfixes a = option a $ do
        a' <- (char '*' $> RStar a)
            <|> (char '+' $> RPlus a)
            <|> (char '?' $> ROpt a)
            <|> (repeatRange a <$> (char '{' *> decimal) <*> optional (char ',' *> optional decimal) <* char '}')
        postfixes a'

    atom = (char '(' *> spaces *> option (RSeq []) (regex env) <* spaces <* char ')')
        <|> regexMacro
        <|> (RString . Text.pack <$> stringLiteral)
        <|> (RSet <$> charSet env)

    regexMacro = do
        name <- char '@' *> identifier
        maybe (fail $ "undefined macro @" ++ Text.unpack name) return (Map.lookup name (envRegexes env))

-- | @r{n}@, @r{n,}@ and @r{n,m}@.
repeatRange :: Regex -> Int -> Maybe (Maybe Int) -> Regex
repeatRange r n bound = RSeq $ replicate n r ++ case bound of
    Nothing       -> []
    Just Nothing  -> [RStar r]
    Just (Just m) -> replicate (m - n) (ROpt r)

-- Character sets

-- | Parses a set, with @#@ for the difference of sets.
charSet :: Env -> Parser CharSet
charSet env = foldl difference <$> simpleSet env <*> many (hspaces *> char '#' *> hspaces *> simpleSet env)

simpleSet :: Env -> Parser CharSet
simpleSet env =
        (complement <$> (char '~' *> simpleSet env))
    <|> bracketed
    <|> (char '.' $> AllBut (Set.singleton '\n'))
    <|> setMacro
    <|> range
  where
    bracketed = do
        _ <- char '['
        inverted <- option False (char '^' $> True)
        items <- many (spaces *> (charSet env <|> setOnlyItem)) <* spaces
        _ <- char ']'
        let s = foldr union (Only Set.empty) items
        return $ if inverted then complement s else s

    -- Inside brackets, characters that are special in regexes are plain.
    setOnlyItem = Only . Set.singleton <$> satisfy (`elem` ("()*+?|/^@\"{};<>" :: String))

    setMacro = do
        name <- char '$' *> identifier
        maybe (fail $ "undefined macro $" ++ Text.unpack name) return (Map.lookup name (envSets env))

    range = do
        from <- setChar
        to <- optional (char '-' *> setChar)
        return $ Only $ maybe (Set.singleton from) (Set.fromList . enumFromTo from) to

-- | Parses a character that stands for itself, or an escape.
setChar :: Parser Char
setChar = escapedChar <|> satisfy (\c -> not (isSpace c) && c `notElem` ("$@[]{}()*+?|/^.~#\"\\;<>-" :: String))

escapedChar :: Parser Char
escapedChar = char '\\' *> choice
    [ char 'x' *> (chr <$> hexadecimal)
    , char 'o' *> (octal =<< takeWhile1 (`elem` ['0' .. '7']))
    , chr <$> decimal
    , char 'n' $> '\n'
    , char 't' $> '\t'
    , char 'r' $> '\r'
    , char 'f' $> '\f'
    , char 'v' $> '\v'
    , char 'b' $> '\b'
    , char 'a' $> '\a'
    , anyChar
    ]
  where
    octal digits = case readOct (Text.unpack digits) of
        [(n, "")] -> return (chr n)
        _         -> fail "bad octal escape"

union :: CharSet -> CharSet -> CharSet
union (Only a) (Only b)     = Only (Set.union a b)
union (Only a) (AllBut b)   = AllBut (Set.difference b a)
union (AllBut a) (Only b)   = AllBut (Set.difference a b)
union (AllBut a) (AllBut b) = AllBut (Set.intersection a b)

difference :: CharSet -> CharSet -> CharSet
difference a b = complement (union (complement a) b)

complement :: CharSet -> CharSet
complement (Only s)   = AllBut s
complement (AllBut s) = Only s

-- Lexical bits

identifier :: Parser Text
identifier = takeWhile1 (\c -> isAlphaNum c || c == '_' || c == '\'')

stringLiteral :: Parser String
stringLiteral = char '"' *> many (escapedChar <|> satisfy (/= '"')) <* char '"'

-- | Parses Haskell code in braces, and returns it without the outer braces.
-- Braces in nested blocks and in string and character literals are kept.
codeBlock :: Parser Text
codeBlock = char '{' *> (Text.concat <$> many piece) <* char '}'
  where
    piece = nested <|> haskellString <|> haskellChar <|> takeWhile1 (`notElem` ("{}\"'" :: String)) <|> string "'"
    nested = (\t -> "{" <> t <> "}") <$> codeBlock
    haskellString = do
        body <- char '"' *> many (escaped <|> (Text.singleton <$> satisfy (/= '"'))) <* char '"'
        return $ "\"" <> Text.concat body <> "\""
    haskellChar = do
        c <- char '\'' *> (escaped <|> (Text.singleton <$> satisfy (/= '\''))) <* char '\''
        return $ "'" <> c <> "'"
    escaped = (\c -> Text.pack ['\\', c]) <$> (char '\\' *> anyChar)

-- | Skips spaces and comments, including newlines.
spaces :: Parser ()
spaces = skipMany (skip isSpace <|> lineComment <|> blockComment)
  where
    lineComment = string "--" *> skipWhile (/= '\n')
    blockComment = string "{-" *> skipMany (blockComment <|> (notChar '-' $> ()) <|> (char '-' *> notChar '}' $> ())) <* string "-}"

-- | Skips spaces and tabs, but not newlines: a regex ends at the end of its line.
hspaces :: Parser ()
hspaces = skipWhile isHorizontalSpace
