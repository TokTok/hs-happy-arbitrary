{-# LANGUAGE OverloadedStrings #-}
-- | Links the tokens of a Happy grammar to the rules of an Alex lexer that
-- make them, and renders generated tokens as source text.
module Language.Happy.Linker
    ( LinkConfig (..)
    , defaultLinkConfig
    , Lexer
    , StartCode
    , TokenGen (..)
    , generateChecked
    , generateSource
    , makeConfig
    , renderChecked
    , renderUnchecked
    ) where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe, mapMaybe)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Language.Alex.Arbitrary  (fixedString, genRegex)
import           Language.Alex.Ast        (AlexFile (..), Regex, Rule (..))
import           Language.Happy.Arbitrary (Config (..), genTokens)
import           Language.Happy.Ast       (Node)
import           Language.Happy.Lexer     (Lexeme)
import           Test.QuickCheck          (Gen, elements, suchThatMaybe)

-- | How to find the token class in Happy and Alex code.
data LinkConfig = LinkConfig
    { happyTokenPattern :: Text -> Maybe Text
      -- ^ The class that a @%token@ pattern matches.
    , alexTokenPattern  :: Text -> Maybe Text
      -- ^ The class that a lexer action makes, or 'Nothing' for none.
    }

-- | Patterns like @{ L _ IdVar _ }@ and actions like @{ mkL IdVar }@.
defaultLinkConfig :: LinkConfig
defaultLinkConfig = LinkConfig
    { happyTokenPattern = wordAfter "L _ "
    , alexTokenPattern  = wordAfter "mkL "
    }

-- | Returns the word after a marker, if the code has the marker.
wordAfter :: Text -> Text -> Maybe Text
wordAfter marker code = case Text.breakOn marker code of
    (_, rest) | Text.null rest -> Nothing
    (_, rest) -> case Text.words (Text.map separate (Text.drop (Text.length marker) rest)) of
        word : _ -> Just word
        []       -> Nothing
  where
    separate c = if c `elem` ("{}`()" :: String) then ' ' else c

-- | The language's lexer: the classes of the tokens in a text, named as the
-- t'LinkConfig' names them, or 'Nothing' if it can't lex the text.
type Lexer = Text -> Maybe [Text]

-- | An Alex start code, like @0@ or @comment@.
type StartCode = Text

-- | How to make the text of one token.
data TokenGen = TokenGen
    { tokenCode  :: Text
      -- ^ The token's @%token@ pattern, without its braces.
    , tokenClass :: Maybe Text
      -- ^ 'Nothing' if 'happyTokenPattern' finds no class in the pattern.
    , genToken   :: StartCode -> Maybe (Gen (String, StartCode))
      -- ^ The token's text in a start code, and the start code after it.
      -- 'Nothing' if no lexer rule makes the token there.
    }

-- | Generates source code that the lexer agrees with. A tree is drawn again if
-- one of its tokens can't be rendered, up to 100 times.
generateChecked :: LinkConfig -> AlexFile -> Lexer -> Text -> Node (Lexeme Text) -> Gen Text
generateChecked cfg alex lexer start happy = go (100 :: Int) ""
  where
    -- Shared between tries, so the grammar is worked out once.
    tokens = genTokens (makeConfig cfg alex) start happy
    go 0 err = error $ "generateChecked: no tree could be rendered in 100 tries, the last one because " ++ Text.unpack err
    go n _ = tokens >>= renderChecked lexer >>= either (go (n - 1)) pure

-- | Generates source code without checking what the lexer makes of it.
generateSource :: LinkConfig -> AlexFile -> Text -> Node (Lexeme Text) -> Gen (Either Text Text)
generateSource cfg alex start happy = genTokens (makeConfig cfg alex) start happy >>= renderUnchecked

-- | Joins the tokens' text so that the lexer finds exactly these tokens again.
-- A token's text and the space before it are drawn again until the lexer
-- agrees. After 20 tries we give up and say which token failed.
renderChecked :: Lexer -> [TokenGen] -> Gen (Either Text Text)
renderChecked lexer = go "" [] "0"
  where
    go out _ _ [] = return $ Right out
    go out classes code (tok : toks) = case (tokenClass tok, genToken tok code) of
        (Nothing, _) -> return $ Left $ unlinked tok
        (Just cls, Nothing) -> return $ Left $ noRule cls code
        (Just cls, Just gen) -> attempt (20 :: Int)
          where
            classes' = classes ++ [cls]
            attempt 0 = return $ Left $ "the lexer never read " <> cls <> " back in start code " <> code
            attempt n = do
                (text, code') <- gen
                space <- if Text.null out then return "" else elements ["", " ", "\n"]
                let out' = out <> space <> Text.pack text
                if lexer out' == Just classes'
                    then go out' classes' code' toks
                    else attempt (n - 1)

-- | Joins the tokens' text with spaces, without checking what a lexer makes of
-- it.
renderUnchecked :: [TokenGen] -> Gen (Either Text Text)
renderUnchecked = go [] "0"
  where
    go out _ [] = return $ Right $ Text.unwords (reverse out)
    go out code (tok : toks) = case (tokenClass tok, genToken tok code) of
        (Nothing, _) -> return $ Left $ unlinked tok
        (Just cls, Nothing) -> return $ Left $ noRule cls code
        (Just _, Just gen) -> do
            (text, code') <- gen
            go (Text.pack text : out) code' toks

unlinked :: TokenGen -> Text
unlinked tok = "the token pattern {" <> tokenCode tok <> "} has no class we know"

noRule :: Text -> StartCode -> Text
noRule cls code = "no lexer rule makes " <> cls <> " in start code " <> code

-- | Rules that make a token class, by start code ('Nothing' for every start
-- code), with the start code they switch to, if any.
type TokenRules = Map (Maybe StartCode, Text) [(Regex, Maybe StartCode)]

-- | Rules that make no token but switch start code, like the @" "+"*"@ at the
-- start of a comment line.
type Switches = Map (Maybe StartCode) [(Regex, StartCode)]

-- | Makes tokens from the grammar's @%token@ patterns, with text from the
-- lexer's rules. A token needs a rule in the current start code, or a chain
-- of rules that only switch start code to get to one.
makeConfig :: LinkConfig -> AlexFile -> Config TokenGen
makeConfig cfg alex = Config $ \code ->
    let cls = happyTokenPattern cfg code
    in TokenGen code cls (\start -> genText start =<< cls)
  where
    (tokenRules, switches) = foldr addRule (Map.empty, Map.empty) (alexRules alex)

    addRule :: Rule -> (TokenRules, Switches) -> (TokenRules, Switches)
    addRule r (toks, sws) = case (alexTokenPattern cfg (ruleCode r), nextStartCode (ruleCode r)) of
        (Just cls, next)     -> (Map.unionWith (++) (Map.fromList [ ((c, cls), [(ruleRegex r, next)]) | c <- codes r ]) toks, sws)
        (Nothing, Just next) -> (toks, Map.unionWith (++) (Map.fromList [ (c, [(ruleRegex r, next)]) | c <- codes r ]) sws)
        (Nothing, Nothing)   -> (toks, sws)
    codes r = if null (ruleStartCodes r) then [Nothing] else map Just (ruleStartCodes r)

    rulesIn table code = Map.findWithDefault [] (Just code) table ++ Map.findWithDefault [] Nothing table

    -- Keywords, so that a regex like @[a-z]+@ doesn't make another class's
    -- keyword (it would lex as that class).
    keywords :: Map String Text
    keywords = Map.fromList $ mapMaybe (\r -> (,) <$> fixedString (ruleRegex r) <*> alexTokenPattern cfg (ruleCode r)) (alexRules alex)

    -- Lists every way to make the class from a start code: the switches to
    -- take first, then the rule that makes it.
    paths :: StartCode -> Text -> [([(Regex, StartCode)], (Regex, Maybe StartCode))]
    paths start cls = go Set.empty [(start, [])]
      where
        go _ [] = []
        go seen ((code, taken) : queue)
            | Set.member code seen = go seen queue
            | otherwise =
                [ (reverse taken, rule) | rule <- Map.findWithDefault [] (Just code, cls) tokenRules ++ Map.findWithDefault [] (Nothing, cls) tokenRules ]
                ++ go (Set.insert code seen) (queue ++ [ (next, (regex, next) : taken) | (regex, next) <- rulesIn switches code ])

    genText :: StartCode -> Text -> Maybe (Gen (String, StartCode))
    genText start cls = case paths start cls of
        [] -> Nothing
        ps -> Just $ do
            (taken, (regex, next)) <- elements ps
            prefix <- concat <$> mapM (genRegex . fst) taken
            text <- fromMaybe <$> genRegex regex <*> (genRegex regex `suchThatMaybe` notOtherKeyword)
            let afterSwitches = if null taken then start else snd (last taken)
            return (prefix ++ text, fromMaybe afterSwitches next)
      where
        notOtherKeyword s = maybe True (== cls) (Map.lookup s keywords)

-- | Returns the start code an action switches to, from @`andBegin` code@ or
-- @begin code@.
nextStartCode :: Text -> Maybe StartCode
nextStartCode code = case wordAfter "`andBegin` " code of
    Just next -> Just next
    Nothing   -> wordAfter "begin " code
