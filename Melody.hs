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
import Report
import Data.Maybe

-- |Analyse a score, applying the melodic analysis rules
analyse :: Music -> [Report]
analyse (Music bs) = catMaybes $ map rule89 $ zip3 [0..] bs $ drop 1 bs

rule89 :: (Int, Beat, Beat) -> Maybe Report
rule89 (i, Beat Rest _, Beat _ _) = Nothing
rule89 (i, Beat _ _, Beat Rest _) = Nothing
rule89 (i, Beat (Note n) _, Beat (Note n') _) | step n n' = Nothing
rule89 (i, Beat (Note n) _, Beat (Note n') _) | consonant (interval n n') = Nothing
rule89 (i, Beat (Note n) _, Beat (Note n') _) | diminished (interval n n') = Nothing -- Leave for next rule
rule89 (i, Beat (Note n) _, Beat (Note n') _) | augmented (interval n n') = Nothing -- Leave for next rule
rule89 (i, Beat (Note n) _, Beat (Note n') _) = Just $ Error (Harmony 89) (Source i (i+1)) $ "Dissonance " ++ show (interval n n')


