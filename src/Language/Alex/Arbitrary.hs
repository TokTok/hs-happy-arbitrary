-- | Random text that a regex matches.
module Language.Alex.Arbitrary
    ( fixedString
    , genRegex
    ) where

import qualified Data.Set          as Set
import qualified Data.Text         as Text
import           Language.Alex.Ast (CharSet (..), Regex (..))
import           Test.QuickCheck   (Gen, elements, listOf, listOf1, oneof,
                                    resize)

-- | The characters we pick from for a set given by what it leaves out.
universe :: [Char]
universe = [' ' .. '~'] ++ "\t\n"

-- | Returns the only string a regex matches, if it matches just one.
fixedString :: Regex -> Maybe String
fixedString r = case r of
    RString t -> Just (Text.unpack t)
    RSet (Only s) | Set.size s == 1 -> Just [Set.findMin s]
    RSet _ -> Nothing
    RStar _ -> Nothing
    RPlus _ -> Nothing
    ROpt _ -> Nothing
    RSeq rs -> concat <$> mapM fixedString rs
    RAlt [] -> Nothing
    RAlt (a : as) -> do
        s <- fixedString a
        if all ((== Just s) . fixedString) as then Just s else Nothing

-- | Makes text that the regex matches. Repetitions are short: at most 5 times.
genRegex :: Regex -> Gen String
genRegex r = resize 5 $ case r of
    RString t -> return $ Text.unpack t
    RSet s -> case members s of
        [] -> error $ "genRegex: " ++ show s ++ " has no characters we can pick"
        cs -> pure <$> elements cs
    RStar r' -> concat <$> listOf (genRegex r')
    RPlus r' -> concat <$> listOf1 (genRegex r')
    ROpt r' -> oneof [return "", genRegex r']
    RSeq rs -> concat <$> mapM genRegex rs
    RAlt rs -> oneof (map genRegex rs)
  where
    members (Only s)   = Set.toList s
    members (AllBut s) = filter (`Set.notMember` s) universe
