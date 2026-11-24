{-# LANGUAGE OverloadedStrings #-}
module Examples.MiniSpec (spec) where

import           Data.Either           (isRight)
import qualified Data.Text             as Text
import           Examples.Files        (readAlexFile, readHappyFile)
import           Examples.Mini.Lexer   (lexClasses, runAlex)
import           Examples.Mini.Parser  (parseProgram)
import           Language.Happy.Linker (Lexer, defaultLinkConfig,
                                        generateChecked)
import           Test.Hspec            (Spec, describe, it, runIO,
                                        shouldSatisfy)
import           Test.QuickCheck       (forAll)

miniLexer :: Lexer
miniLexer = either (const Nothing) (Just . map (Text.pack . show)) . lexClasses . Text.unpack

spec :: Spec
spec = describe "generateChecked" $ do
    alex <- runIO $ readAlexFile "test/Examples/Mini/Lexer.x"
    happy <- runIO $ readHappyFile "test/Examples/Mini/Parser.y"

    it "makes programs that the language's own parser accepts" $
        forAll (generateChecked defaultLinkConfig alex miniLexer "Program" happy) $ \code ->
            runAlex (Text.unpack code) parseProgram `shouldSatisfy` isRight
