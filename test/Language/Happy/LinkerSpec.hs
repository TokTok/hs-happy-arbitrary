{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.LinkerSpec (spec) where

import qualified Data.ByteString.Lazy  as LBS
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Language.Alex.Ast     (AlexFile)
import           Language.Alex.Parser  (parseAlex)
import           Language.Happy.Ast    (Node)
import           Language.Happy.Lexer  (Lexeme, runAlex)
import           Language.Happy.Linker (Lexer, defaultLinkConfig,
                                        generateChecked, generateSource)
import           Language.Happy.Parser (parseGrammar)
import           Test.Hspec            (Spec, describe, it, shouldBe,
                                        shouldSatisfy)
import           Test.QuickCheck       (forAll, generate, vectorOf)

alexFile :: [Text] -> AlexFile
alexFile = either error id . parseAlex . Text.unlines

happyFile :: [Text] -> Node (Lexeme Text)
happyFile = either error id . flip runAlex parseGrammar . LBS.fromStrict . Text.encodeUtf8 . Text.unlines

-- | Words, where @if@ is a keyword, and comments with words in them.
wordsLexer :: AlexFile
wordsLexer = alexFile
    [ "tokens :-"
    , "<0> \"if\" { mkL KwIf }"
    , "<0> [a-z]+ { mkL IdVar }"
    , "<0> \"/*\" { mkL CmtStart `andBegin` cmt }"
    , "<cmt> [a-z]+ { mkL CmtWord }"
    , "<cmt> \"*/\" { mkL CmtEnd `andBegin` 0 }"
    , "\" \"+ ;"
    ]

wordsGrammar :: [Text] -> Node (Lexeme Text)
wordsGrammar rules = happyFile $
    [ "{ module Words where }"
    , "%token"
    , "    if { L _ KwIf _ }"
    , "    ID { L _ IdVar _ }"
    , "    '/*' { L _ CmtStart _ }"
    , "    WORD { L _ CmtWord _ }"
    , "    '*/' { L _ CmtEnd _ }"
    , "    NONE { L _ Missing _ }"
    , "    ODD { Odd }"
    , "%%"
    ] ++ rules ++ ["{ }"]

-- | What the lexer above reads in text that only has words and spaces.
readWords :: Lexer
readWords text
    | Text.all (\c -> c == ' ' || (c >= 'a' && c <= 'z')) text = Just [ if w == "if" then "KwIf" else "IdVar" | w <- Text.words text ]
    | otherwise = Nothing

isComment :: [Text] -> Bool
isComment ["/*", _, "*/"] = True
isComment _               = False

spec :: Spec
spec = do
    describe "generateSource" $ do
        it "doesn't make another token's keyword" $ do
            results <- generate $ vectorOf 200 $ generateSource defaultLinkConfig wordsLexer "S" (wordsGrammar ["S : ID { () }"])
            results `shouldSatisfy` all (either (const False) (/= "if"))

        it "switches start code before a token that needs it" $ do
            result <- generate $ generateSource defaultLinkConfig wordsLexer "S" (wordsGrammar ["S : '/*' WORD '*/' { () }"])
            fmap Text.words result `shouldSatisfy` either (const False) isComment

        it "says which token has no lexer rule" $ do
            result <- generate $ generateSource defaultLinkConfig wordsLexer "S" (wordsGrammar ["S : NONE { () }"])
            result `shouldBe` Left "no lexer rule makes Missing in start code 0"

        it "says which token has no class" $ do
            result <- generate $ generateSource defaultLinkConfig wordsLexer "S" (wordsGrammar ["S : ODD { () }"])
            result `shouldBe` Left "the token pattern { Odd } has no class we know"

    describe "generateChecked" $
        it "keeps tokens apart that would lex as one" $
            forAll (generateChecked defaultLinkConfig wordsLexer readWords "S" (wordsGrammar ["S : ID ID { () }"])) $ \code ->
                readWords code `shouldBe` Just ["IdVar", "IdVar"]
