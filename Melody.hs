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
analyse (Music ps) = catMaybes $ concat $ applyRules $ partZippers ps
  where
    partZippers = map (Z.fromList . annotated)
    applyRules zs = walkZipper <$> rules <*> zs
    rules = [ruleH89, ruleH90, ruleH91, ruleH92]


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

-- Analysis of Music according to Section 90 in Prouts Harmony
-- A diminished interval must be resolved correctly
ruleH90 :: Z.Zipper ANote -> Maybe R.Report
ruleH90 z
  | diminished i = evaluate
  | otherwise    = Nothing
    where
      (i, part, s, e) = getBasicInfo z
      (l2, l, r, r2) = getContext z
      evaluate
        | isNothing r2 = Just $ R.Warning (R.Harmony 90) (R.Source [part] s e) $ show i
        | outside l r (fromJust r2) = Just $ R.Error (R.Harmony 90) (R.Source [part] s e) $ "Outside " ++ show i
        | resolved l r $ fromJust r2 = Nothing
        | otherwise = Just $ R.Error (R.Harmony 90) (R.Source [part] s e) $ "Unresolved " ++ show i
      resolved a1 a2 a = second i && minor i -- Resolution to a diminished is a semitone in from the last note
        where
          [n1, n2, n] = fmap (note . event) [a1, a2, a]
          i = interval n2 n

-- Analysis of Music according to Section 91 in Prouts Harmony
-- An augmented interval must be resolved correctly
ruleH91 :: Z.Zipper ANote -> Maybe R.Report
ruleH91 z
  | second i    = Nothing
  | augmented i = Just $ R.Error (R.Harmony 91) (R.Source [part] s e) $ show i
  | otherwise   = Nothing
    where
      (i, part, s, e) = getBasicInfo z

-- Analysis of Music according to Section 92 in Prouts Harmony
-- A large interval must be approached and quitted from inside the interval
ruleH92 :: Z.Zipper ANote -> Maybe R.Report
ruleH92 z
  | not $ large i   = Nothing
  | isNote l2 && outside l r (fromJust l2) = Just $ R.Error (R.Harmony 92) (R.Source [part] s e) $ "Large Interval Approach"
  | isNote r2 && outside l r (fromJust r2) = Just $ R.Error (R.Harmony 92) (R.Source [part] s e) $ "Large Interval Leave"
  | otherwise       = Nothing
    where
      (i, part, s, e) = getBasicInfo z
      (l2, l, r, r2) = getContext z
      isNote (Just (ANote _ _ _  (Play _ _))) = True
      isNote _ = False


outside :: ANote -> ANote -> ANote -> Bool
outside a1 a2 a = n <= min n1 n2 || n >= max n1 n2
  where
    [n1, n2, n] = fmap (note . event) [a1, a2, a]

note :: Event -> Note
note (Play _ n) = n


getBasicInfo :: Z.Zipper ANote -> (Interval, PartName, Time, Time)
getBasicInfo z = (interval (note l) (note r), part l, start l, end r)
  where
    note (ANote _ _ _ (Play _ n)) = n
    (_, l, r, _) = getContext z

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

walkZipper :: (Z.Zipper ANote -> b) -> Z.Zipper ANote -> [b]
walkZipper f z
  | Z.endp z = []
  | Z.endp $ Z.right z = []
  | rest z = recurse
  | rest (Z.right z) = recurse
  | otherwise = apply : recurse
    where
      rest = isRest . event . Z.cursor 
      isRest (Rest _) = True
      isRest _ = False
      apply = f z
      recurse = walkZipper f $ Z.right z
