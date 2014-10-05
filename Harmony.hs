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
analyse (Music ps) = concatMap (analysePart . Z.fromList . annotated) $ ps
  where
    analysePart z = catMaybes $ concat $ mapPairs <$> rules <*> (splitZipper rests z)
    rests a a' = isRest (event a) || isRest (event a')
    isRest (Rest _) = True
    isRest _ = False
    rules = [ruleH96]


-- Analysis of Music according to Section 96 in Prouts Harmony
-- Consecutive unisons are bad
ruleH96 :: Z.Zipper ANote -> Maybe R.Report
ruleH96 z
  | otherwise     = Just $ R.Error (R.Harmony 96) (R.Source part s e) $ "Consecutive unisons"
    where
      (i, part, s, e) = getBasicInfo z


data ANote = ANote { start :: Time, end :: Time, part :: PartName, event :: Event }

annotated :: Part -> [ANote]
annotated (Part p es) = snd $ foldl ann (0, []) es
  where
    ann (t, as) e = (t + dur e, as ++ [ANote t (t + dur e) p e])
    dur (Rest d) = d
    dur (Note d _) = d

getBasicInfo :: Z.Zipper ANote -> (Interval, PartName, Time, Time)
getBasicInfo z = (interval (note l) (note r), part l, start l, end r)
  where
    note (ANote _ _ _ (Note _ n)) = n
    (_, l, r, _) = getContext z

getContext :: Z.Zipper a -> (Maybe a, a, a, Maybe a)
getContext z = (l2, l, r, r2)
  where
        l2 = if Z.beginp z then Nothing else Z.safeCursor $ Z.left z
        l = Z.cursor z
        r = Z.cursor $ Z.right z
        r2 = Z.safeCursor $ Z.right $ Z.right z

splitZipper :: (a -> a -> Bool) -> Z.Zipper a -> [Z.Zipper a]
splitZipper p z@(Z.Zip ls rs)
  | Z.endp z = [Z.fromList $ reverse ls]
  | Z.beginp z = splitZipper p $ Z.right z
  | otherwise = if p (head ls) (head rs) then Z.fromList (reverse ls) : splitZipper p (Z.fromList rs) else splitZipper p $ Z.right z

mapPairs :: (Z.Zipper a -> b) -> Z.Zipper a -> [b]
mapPairs f = Z.foldrz foldit []
  where
    foldit z rs = if Z.endp $ Z.right z then rs else f z : rs
