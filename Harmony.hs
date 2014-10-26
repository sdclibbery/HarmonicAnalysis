{-|
Module      : Harmony
Description : Provide a harmonic analysis of some Music

Provide analysis of purely sequential (harmonic) music.
No analysis of note against note (ie harmonic or contrapuntal) is undertaken in this module.
The analytic rules are according to Ebeneezer Prouts books 'Harmony' and 'Counterpoint'.
-}

module Harmony (
  analyse
) where
import Note
import Interval
import Structure
import qualified Report as R
import Data.Maybe
import qualified Data.List.Zipper as Z
import Control.Applicative


-- |Analyse a score, applying the harmonic analysis rules
analyse :: Music -> [R.Report]
analyse (Music ps) = catMaybes $ concat $ applyRules $ pairsOfPartZippers ps
  where
    pairsOfPartZippers = allPairs . map (Z.fromList . annotated)
    applyRules zps = walkZippers <$> rules <*> zps
    rules = [ruleH96]

-- Analysis of Music according to Section 96 in Prouts Harmony
-- Consecutive unisons are bad
ruleH96 :: (Z.Zipper ANote, Z.Zipper ANote) -> Maybe R.Report
ruleH96 (z, z')
  | unison i && unison i2 = Just $ R.Error (R.Harmony 96) (R.Source ps s e) $ "Consecutive unisons"
  | otherwise             = Nothing
  where
    (i, i2, ps, s, e) = getBasicInfo z z'


getBasicInfo :: Z.Zipper ANote -> Z.Zipper ANote -> (Interval, Interval, [PartName], Time, Time)
getBasicInfo z z' = (interval (note l) (note l'), interval (note r) (note r'), [part l, part l'], s, e)
  where
    note (ANote _ _ _ (Play _ n)) = n
    (_, l, r, _) = getContext z
    (_, l', r', _) = getContext z'
    s = max (start l) (start l')
    e = min (end r) (end r')

getContext :: Z.Zipper a -> (Maybe a, a, a, Maybe a)
getContext z = (l2, l, r, r2)
  where
        l2 = if Z.beginp z then Nothing else Z.safeCursor $ Z.left z
        l = Z.cursor z
        r = Z.cursor $ Z.right z
        r2 = Z.safeCursor $ Z.right $ Z.right z


data ANote = ANote { start :: Time, end :: Time, part :: PartName, event :: Event }

annotated :: Part -> [ANote]
annotated (Part p es) = snd $ foldl ann (0, []) es
  where
    ann (t, as) e = (t + dur e, as ++ [ANote t (t + dur e) p e])
    dur (Rest d) = d
    dur (Play d _) = d

walkZippers :: ((Z.Zipper ANote, Z.Zipper ANote) -> b) -> (Z.Zipper ANote, Z.Zipper ANote) -> [b]
walkZippers f (z, z')
  | Z.endp z || Z.endp z' = []
  | (Z.endp $ Z.right z) || (Z.endp $ Z.right z') = []
  | rest z || rest z' = recurse
  | rest (Z.right z) || rest (Z.right z') = recurse
  | otherwise = apply : recurse
    where
      rest = isRest . event . Z.cursor 
      isRest (Rest _) = True
      isRest _ = False
      apply = f (z, z')
      recurse
        | (end $ Z.cursor z) < (end $ Z.cursor z') = walkZippers f (Z.right z, z')
        | (end $ Z.cursor z) > (end $ Z.cursor z') = walkZippers f (z, Z.right z')
        | otherwise                                = walkZippers f (Z.right z, Z.right z')

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs xxs@(x:xs) = zip (repeat x) xs ++ allPairs xs
