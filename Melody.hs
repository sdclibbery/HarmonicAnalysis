{-|
Module      : Melody
Description : Provide a melodic analysis of some Music

Provide analysis of purely sequential (melodic) music.
No analysis of note against note (ie harmonic or contrapuntal) is undertaken in this module.
The analytic rules are according to Ebeneezer Prouts books 'Harmony' and 'Counterpoint'.
-}

module Melody (
  analyse
) where
import Note
import Interval
import Structure
import qualified Report as R
import Data.Maybe
import qualified Data.List.Zipper as Z
import Control.Applicative


-- |Analyse a score, applying the melodic analysis rules
analyse :: Music -> [R.Report]
analyse (Music ps) = concatMap (analysePart . Z.fromList . annotated) $ ps
  where
    analysePart z = catMaybes $ concat $ mapPairs <$> rules <*> (splitZipper rests z)
    rests a a' = isRest (event a) || isRest (event a')
    isRest (Rest _) = True
    isRest _ = False
    rules = [ruleH89]


-- Analysis of Music according to Section 89 in Prouts Harmony
-- Any dissonance other than a second is bad
ruleH89 :: Z.Zipper ANote -> Maybe R.Report
ruleH89 z
  | step i     = Nothing
  | consonant i   = Nothing
  | diminished i  = Nothing -- Leave for rule 90
  | augmented i   = Nothing -- Leave for rule 91
  | otherwise     = Just $ R.Error (R.Harmony 89) (R.Source [part] s e) $ "Dissonance " ++ show i
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
