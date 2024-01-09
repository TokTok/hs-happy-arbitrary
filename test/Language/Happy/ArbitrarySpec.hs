{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Happy.ArbitrarySpec where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Language.Happy.Arbitrary (genTokens)
import           Language.Happy.Ast       (Node)
import           Language.Happy.Lexer     (Lexeme, runAlex)
import           Language.Happy.Parser    (parseGrammar, source)
import           Language.Happy.Tokens    (LexemeClass (..))
import           Test.Hspec               (Spec, describe, expectationFailure,
                                           it)
import           Test.QuickCheck          (forAll)


sampleToken :: LexemeClass -> Text
sampleToken c = case c of
    IdName             -> "name"

    KwErrorhandlertype -> "%errorhandlertype"
    KwError            -> "%error"
    KwExpect           -> "%expect"
    KwLeft             -> "%left"
    KwLexer            -> "%lexer"
    KwMonad            -> "%monad"
    KwName             -> "%name"
    KwRight            -> "%right"
    KwToken            -> "%token"
    KwTokentype        -> "%tokentype"

    PctColon           -> ":"
    PctColonColon      -> "::"
    PctPercentPercent  -> "%%"
    PctPipe            -> "|"

    LitCode            -> "{ code }"
    LitInteger         -> "0"
    LitString          -> "'token'"

    ErrorToken         -> "!!!ERROR!!!"
    Eof                -> "!!!EOF!!!"

parseToken :: Text -> LexemeClass
parseToken = read . Text.unpack . (!! 2) . concatMap (filter (not . Text.null) . Text.splitOn "\t") . Text.splitOn " "

tryParseGrammar :: Monad m => (Node (Lexeme Text) -> m ()) -> m ()
tryParseGrammar f =
    case runAlex (LBS.fromStrict source) parseGrammar of
        Left _ | BS.null source -> return ()
        Left err                -> error err
        Right ok                -> f ok

spec :: Spec
spec = tryParseGrammar $ \g -> do
    describe "stuff" $ do
        it "does a thing" $
            forAll (Text.intercalate " " . map (sampleToken . parseToken) <$> genTokens "Grammar" g) $ \code -> do
                case runAlex (LBS.fromStrict . Text.encodeUtf8 $ code) parseGrammar of
                    Left err -> expectationFailure err
                    Right ok -> print ok
