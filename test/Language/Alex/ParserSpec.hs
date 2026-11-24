{-# LANGUAGE OverloadedStrings #-}
module Language.Alex.ParserSpec (spec) where

import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Language.Alex.Ast
import           Language.Alex.Parser (parseAlex)
import           Test.Hspec           (Spec, describe, it, shouldBe,
                                       shouldSatisfy)

-- | The regexes of the rules in a file.
regexes :: [Text] -> Either String [Regex]
regexes = fmap (map ruleRegex . alexRules) . parseAlex . Text.unlines

only :: String -> Regex
only = RSet . Only . Set.fromList

spec :: Spec
spec = describe "parseAlex" $ do
    it "expands set macros, also inside brackets" $
        regexes
            [ "$lower = [a-z]"
            , "$digit = 0-9"
            , "tokens :-"
            , "[$lower $digit \\_]+ ;"
            ] `shouldBe` Right [RPlus (only (['a' .. 'z'] ++ ['0' .. '9'] ++ "_"))]

    it "takes the difference and complement of sets" $
        regexes
            [ "$consonant = [a-z] # [aeiou]"
            , "tokens :-"
            , "$consonant ;"
            , "~$consonant ;"
            , "[^a] ;"
            , ". ;"
            ] `shouldBe` Right
                [ only (filter (`notElem` ("aeiou" :: String)) ['a' .. 'z'])
                , RSet (AllBut (Set.fromList (filter (`notElem` ("aeiou" :: String)) ['a' .. 'z'])))
                , RSet (AllBut (Set.singleton 'a'))
                , RSet (AllBut (Set.singleton '\n'))
                ]

    it "expands regex macros" $
        regexes
            [ "@word = [a-z]+"
            , "tokens :-"
            , "@word \"!\" ;"
            ] `shouldBe` Right [RSeq [RPlus (only ['a' .. 'z']), RString "!"]]

    it "repeats as many times as it says" $
        regexes
            [ "tokens :-"
            , "a{2} ;"
            , "a{1,} ;"
            , "a{1,3} ;"
            ] `shouldBe` Right
                [ RSeq [only "a", only "a"]
                , RSeq [only "a", RStar (only "a")]
                , RSeq [only "a", ROpt (only "a"), ROpt (only "a")]
                ]

    it "reads start-code blocks, contexts and comments" $
        fmap alexRules (parseAlex (Text.unlines
            [ "{ module Lexer where }"
            , "%wrapper \"monad\""
            , "{- a {- nested -} comment -}"
            , "lexer :-"
            , "<cmt> {"
            , "  \"x\" / \"y\" { mkL X } -- a comment"
            , "  ^\"z\"$ ;"
            , "}"
            , "{ trailer }"
            ])) `shouldBe` Right
                [ Rule ["cmt"] (RString "x") " mkL X "
                , Rule ["cmt"] (RString "z") ";"
                ]

    it "keeps braces in strings and characters in actions" $
        fmap (map ruleCode . alexRules) (parseAlex (Text.unlines
            [ "tokens :-"
            , "\"x\" { f \"}\" '{' }"
            ])) `shouldBe` Right [" f \"}\" '{' "]

    it "rejects a macro that isn't defined" $
        regexes ["tokens :-", "[$nope] ;"] `shouldSatisfy` either (const True) (const False)
