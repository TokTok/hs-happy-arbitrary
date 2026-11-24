{-# LANGUAGE OverloadedStrings #-}
-- | Prints random programs for a Happy grammar and Alex lexer, or with only a
-- lexer, sample text for each token class.
module Main (main) where

import           Control.Monad            (forM, forM_, replicateM, unless)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.List                (sortOn)
import qualified Data.Map                 as Map
import           Data.Ord                 (Down (..))
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO             as Text
import           Language.Alex.Arbitrary  (fixedString, genRegex)
import           Language.Alex.Ast        (AlexFile (..), Regex (..), Rule (..))
import           Language.Alex.Parser     (parseAlex)
import           Language.Happy.Arbitrary (genTree)
import           Language.Happy.GenTree   (flatten, getRules)
import           Language.Happy.Grammar   (Sym (..), fromAst)
import           Language.Happy.Lexer     (runAlex)
import           Language.Happy.Linker    (LinkConfig (..), defaultLinkConfig,
                                           makeConfig, renderUnchecked)
import           Language.Happy.Parser    (parseGrammar)
import           Options.Applicative
import           System.Exit              (die)
import           Test.QuickCheck          (generate, resize)
import           Text.Printf              (printf)

data Options = Options
    { alexFile   :: FilePath
    , happyFile  :: Maybe FilePath
    , startSym   :: Text
    , size       :: Int
    , iterations :: Int
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption (long "alex" <> metavar "FILE" <> help "Alex lexer (.x)")
    <*> optional (strOption (long "happy" <> metavar "FILE" <> help "Happy grammar (.y); without it, prints sample text for each token class"))
    <*> strOption (long "start" <> metavar "RULE" <> value "TranslationUnit" <> showDefault <> help "Rule to generate")
    <*> option auto (long "size" <> metavar "TOKENS" <> value 100 <> showDefault <> help "Size of each program in tokens")
    <*> option auto (long "iterations" <> metavar "N" <> value 1 <> showDefault <> help "Number of programs")

main :: IO ()
main = do
    opts <- execParser $ info (optionsParser <**> helper)
        (fullDesc <> progDesc "Generates random programs from Happy and Alex files.")
    alexText <- Text.decodeUtf8With lenientDecode <$> BS.readFile (alexFile opts)
    alex <- either (die . ("Can't parse the Alex file: " ++)) return (parseAlex alexText)
    case happyFile opts of
        Nothing   -> printTokenSamples alex
        Just path -> printPrograms alex path opts

-- | Prints text for each token class: the class's only text, or 5 samples.
printTokenSamples :: AlexFile -> IO ()
printTokenSamples alex = do
    let byClass = Map.fromListWith (flip (++))
            [ (cls, [ruleRegex r]) | r <- alexRules alex, Just cls <- [alexTokenPattern defaultLinkConfig (ruleCode r)] ]
    forM_ (Map.toList byClass) $ \(cls, regexes) -> do
        let regex = RAlt regexes
        samples <- maybe (show <$> generate (replicateM 5 (genRegex regex))) (return . show) (fixedString regex)
        printf "%-24s %s\n" (Text.unpack cls) samples

-- | Prints programs, then which rules they used.
printPrograms :: AlexFile -> FilePath -> Options -> IO ()
printPrograms alex path opts = do
    grammar <- LBS.readFile path
    happy <- either (die . ("Can't parse the Happy file: " ++)) return (runAlex grammar parseGrammar)
    let config = makeConfig defaultLinkConfig alex
    used <- forM [1 .. iterations opts] $ \_ -> do
        (code, rules) <- generate $ resize (size opts) $ do
            tree <- genTree config (startSym opts) happy
            code <- renderUnchecked (flatten tree)
            return (code, getRules tree)
        either (\err -> Text.putStrLn ("<" <> err <> ">")) (BS.putStr . Text.encodeUtf8 . (<> "\n")) code
        return rules

    let counts = Map.fromListWith (+) [ (r, 1 :: Int) | r <- concat used ]
        allRules = Set.fromList [ name | Sym name _ <- Map.keys (fromAst (startSym opts) happy) ]
        never = Set.difference allRules (Map.keysSet counts)
    printf "\n%d of %d rules used\n" (Map.size counts) (Set.size allRules)
    unless (Set.null never) $
        putStrLn $ "Never used: " ++ unwords (map Text.unpack (Set.toList never))
    putStrLn $ "Most used: " ++ unwords [ Text.unpack r ++ "=" ++ show c | (r, c) <- take 12 (sortOn (Down . snd) (Map.toList counts)) ]
