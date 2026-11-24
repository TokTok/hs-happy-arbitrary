{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.CimpleSpec (spec) where

import qualified Data.Map                 as Map
import           Data.Maybe               (isJust)
import           Examples.Files           (readAlexFile, readHappyFile)
import           Language.Alex.Ast        (AlexFile (..), Rule (..))
import           Language.Happy.Arbitrary (Config (..))
import qualified Language.Happy.Grammar   as Grammar
import           Language.Happy.Linker    (TokenGen (..), defaultLinkConfig,
                                           makeConfig)
import           Test.Hspec               (Spec, describe, it, runIO, shouldBe)

-- | Cimple's lexer and grammar: a real language with many start codes.
spec :: Spec
spec = describe "Cimple" $ do
    alex <- runIO $ readAlexFile "test/examples/Cimple/Lexer.x"
    happy <- runIO $ readHappyFile "test/examples/Cimple/Parser.y"

    it "has a lexer rule for every token in some start code" $ do
        let Config{parseToken} = makeConfig defaultLinkConfig alex
            startCodes = "0" : concatMap ruleStartCodes (alexRules alex)
            unlinked = [ tokenCode tok
                       | tok <- map parseToken (Map.elems (Grammar.extractTokens id happy))
                       , not (any (isJust . genToken tok) startCodes)
                       ]
        unlinked `shouldBe` []
