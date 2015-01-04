{-|
Module      : Chord
Description : Definition and inspection of a chord
-}
module Chord (
	Chord,
	intervalsToChord,
	notesToChord,
	chordToNotes
) where
import Note
import Interval
import Data.List
import Data.Ord

-- |Definition of a musical chord: bass note plus intervals to other notes
data Chord = Chord Note [Interval] deriving (Show, Eq)

-- |Build a bass note and a set of intervals into a valid chord
intervalsToChord :: Note -> [Interval] -> Chord
intervalsToChord bass is = Chord bass $ sortBy (comparing chr) $ map normalise is

-- |Build a bass note and a set of other notes into a valid chord
notesToChord :: Note -> [Note] -> Chord
notesToChord bass ns = intervalsToChord bass $ map (interval bass) ns

-- |Get the list of notes (starting with the bass) for a given chord
chordToNotes :: Chord -> [Note]
chordToNotes (Chord bass is) = bass : map (applyInterval bass) is
