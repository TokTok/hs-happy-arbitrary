-- | Reads the example languages' files from Bazel's or cabal's working
-- directory.
module Examples.Files
    ( readAlexFile
    , readHappyFile
    ) where

import qualified Data.ByteString.Lazy  as LBS
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as Text
import           Language.Alex.Ast     (AlexFile)
import           Language.Alex.Parser  (parseAlex)
import           Language.Happy.Ast    (Node)
import           Language.Happy.Lexer  (Lexeme, runAlex)
import           Language.Happy.Parser (parseGrammar)
import           System.Directory      (doesFileExist)

readTestFile :: FilePath -> IO LBS.ByteString
readTestFile path = do
    let bazelPath = "hs-happy-arbitrary/" ++ path
    inBazel <- doesFileExist bazelPath
    LBS.readFile (if inBazel then bazelPath else path)

readAlexFile :: FilePath -> IO AlexFile
readAlexFile path = either (fail . ((path ++ ": ") ++)) return . parseAlex . Text.decodeUtf8 . LBS.toStrict =<< readTestFile path

readHappyFile :: FilePath -> IO (Node (Lexeme Text))
readHappyFile path = either (fail . ((path ++ ": ") ++)) return . flip runAlex parseGrammar =<< readTestFile path
