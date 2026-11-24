{-# LANGUAGE StrictData #-}
-- | Trees of a given size in tokens. Every symbol knows the fewest and the
-- most tokens it can make. A node picks one of the productions that can make
-- its budget, and shares the budget among its children so that each of them
-- can make its share. Both favour symbols that reach more of the grammar.
module Language.Happy.Sized
    ( Range (..)
    , genSized
    , ranges
    ) where

import           Control.Monad          (zipWithM)
import           Data.List              (minimumBy, sortOn)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Data.Ord               (comparing)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import           Language.Happy.Ast     (Sym (..))
import           Language.Happy.GenTree (GenTree (..))
import           Language.Happy.Grammar (Rules)
import           Test.QuickCheck        (Gen, choose, shuffle)

-- | The fewest and the most tokens a symbol makes. 'Nothing' as the most
-- means there's no limit.
data Range = Range Int (Maybe Int)
    deriving (Show, Eq)

-- | Returns the size range of every nonterminal that makes a finite tree.
ranges :: Rules -> Set Text -> Map Sym Range
ranges grammar terminals = Map.intersectionWith Range mins maxes
  where
    size m s@(Sym name _)
        | Set.member name terminals = Just 1
        | otherwise = Map.lookup s m
    best pick m = Map.mapMaybe (\prods -> case mapMaybe (fmap sum . mapM (size m)) prods of
        [] -> Nothing
        ns -> Just (pick ns)) grammar

    -- The smallest tree has no path with a nonterminal on it twice, so as
    -- many rounds as there are nonterminals are enough.
    mins = iterate (best minimum) Map.empty !! (Map.size grammar + 1)

    -- With n nonterminals, a most with a limit settles within n rounds. One
    -- without a limit keeps growing, at least once every n rounds, because no
    -- cycle is longer than n.
    rounds = iterate (best maximum) (0 <$ mins)
    settled = rounds !! (Map.size grammar + 1)
    later = rounds !! (2 * Map.size grammar + 2)
    maxes = Map.mapWithKey (\s n -> if Map.lookup s later == Just n then Just n else Nothing) settled

-- | Draws a tree of the start symbol with @n@ tokens, or as close to it as the
-- grammar allows.
genSized :: Rules -> Map Text token -> Sym -> Int -> Gen (GenTree token)
genSized grammar tokens start = \n -> build (clamp n (rangeOf start)) start
  where
    rs = ranges grammar (Map.keysSet tokens)
    reached = reach grammar

    -- The productions of every nonterminal that make finite trees, with their
    -- size ranges and how likely they are: the square root of how many
    -- symbols they reach, so that productions with more in them come up more
    -- often without crowding out the small ones.
    options = Map.map (\prods -> [ (prod, r, sqrt (fromIntegral (Set.size (Set.unions (map reachOf prod))))) | prod <- prods, Just r <- [prodRange prod] ]) grammar
    reachOf s = Map.findWithDefault (Set.singleton s) s reached

    rangeOf s@(Sym name _)
        | Map.member name tokens = Range 1 (Just 1)
        | otherwise = Map.findWithDefault (error $ "genSized: " ++ show s ++ " makes no finite tree") s rs

    -- 'Nothing' if a symbol makes no finite tree.
    prodRange prod = foldr add (Range 0 (Just 0)) <$> mapM symRange prod
    symRange s@(Sym name _)
        | Map.member name tokens = Just (Range 1 (Just 1))
        | otherwise = Map.lookup s rs
    add (Range a b) (Range c d) = Range (a + c) ((+) <$> b <*> d)

    build b sym@(Sym name _) = case Map.lookup name tokens of
        Just token -> return $ Leaf token
        Nothing -> do
            let candidates = Map.findWithDefault [] sym options
                fitting = [ (w, (prod, r)) | (prod, r, w) <- candidates, fits b r ]
            -- A size between the fewest and the most isn't always possible.
            (prod, r) <- if null fitting
                then return $ minimumBy (comparing (distance b . snd)) [ (prod, r) | (prod, r, _) <- candidates ]
                else pickWeighted fitting
            shares <- split (clamp b r) [ (rangeOf s, Set.size (reachOf s)) | s <- prod ]
            Node sym <$> zipWithM build shares prod

    fits b (Range lo hi) = lo <= b && maybe True (>= b) hi
    distance b (Range lo hi) = max (lo - b) (maybe 0 (b -) hi)
    clamp b (Range lo hi) = max lo (maybe b (min b) hi)

-- | Picks one of the values with a probability proportional to its weight.
pickWeighted :: [(Double, a)] -> Gen a
pickWeighted options = do
    r <- choose (0, sum (map fst options))
    return $ pick r options
  where
    pick _ [(_, a)] = a
    pick r ((w, a) : rest)
        | r <= w = a
        | otherwise = pick (r - w) rest
    pick _ [] = error "pickWeighted: no options"

-- | Returns the symbols each nonterminal can reach, itself included.
reach :: Rules -> Map Sym (Set Sym)
reach grammar = iterate step (Map.mapWithKey (\s _ -> Set.singleton s) grammar) !! (Map.size grammar + 1)
  where
    step sets = Map.mapWithKey (\s prods -> Set.insert s (Set.unions [ Map.findWithDefault (Set.singleton c) c sets | c <- concat prods ])) grammar

-- | Shares a budget among children with these ranges and weights, in random
-- order, so that the children after each one can still make what's left. A
-- child gets its fewest tokens plus a part of the rest that grows with its
-- weight (times a random factor): a list of one token can take any number of
-- tokens, but a tree full of it tests less than one with more in it.
split :: Int -> [(Range, Int)] -> Gen [Int]
split budget children = do
    order <- shuffle (zip [0 :: Int ..] children)
    weighted <- mapM (\(i, (r, n)) -> (\u -> (i, r, fromIntegral n * u)) <$> choose (0, 1 :: Double)) order
    return $ map snd . sortOn fst $ go budget weighted
  where
    go _ [] = []
    go left ((i, Range lo hi, w) : rest) = (i, share) : go (left - share) rest
      where
        restLo = sum [ l | (_, Range l _, _) <- rest ]
        restHi = sum <$> sequence [ h | (_, Range _ h, _) <- rest ]
        from = max lo (maybe lo (left -) restHi)
        to = maybe id min hi (left - restLo)
        spare = left - lo - restLo
        total = w + sum [ v | (_, _, v) <- rest ]
        wanted = lo + if total > 0 then round (fromIntegral spare * w / total) else 0
        share = max from (min to wanted)
