{-|
Module      : Numeral
Description : Definition and inspection of a chord as represented by a roman numeral
-}
module Numeral (
	Root(..),
	Inversion(..),
	Numeral(..),
	numeralToChord,
	rootNote
) where
import Key
import Note
import Interval
import Intervals
import Chord

-- |Root of a chord
data Root = I | II | III | IV | V | VI | VII deriving (Show, Eq, Ord, Enum)

-- |Chord inversion
data Inversion = First | Second | Third | Fourth | Fifth | Sixth deriving (Show, Eq, Ord, Enum)

-- |Definition of a Roman Numeral representation of a chord
data Numeral = Numeral Root Alter [Interval] Inversion deriving (Show, Eq)

numeralToChord :: Key -> Numeral -> Chord
numeralToChord k h@(Numeral r a is inv) = chordFrom $ relocate $ rotate (fromEnum inv) notes
  where
    baseOctave = 3
    root = rootNote k baseOctave h
    notes = root : (map (applyInterval root) is)
    chordFrom ns = notesToChord (head ns) (tail ns)
    rotate n xs = take (length xs) (drop n (cycle xs))
    relocate ns = if fromEnum inv + fromEnum r > 3 then map (modifyOctave (-1)) ns else ns

rootNote :: Key -> Int -> Numeral -> Note
rootNote (Key d a q) o (Numeral r al _ _) = applyInterval (Note d a o) (rootToInterval r al q)
  where
    rootToInterval I Fl KeyMajor = _d1
    rootToInterval I Nat KeyMajor = _P1
    rootToInterval I Sh KeyMajor = _a1
    rootToInterval II Fl KeyMajor = _m2
    rootToInterval II Nat KeyMajor = _M2
    rootToInterval II Sh KeyMajor = _a2
    rootToInterval III Fl KeyMajor = _m3
    rootToInterval III Nat KeyMajor = _M3
    rootToInterval III Sh KeyMajor = _a3
    rootToInterval IV Fl KeyMajor = _d4
    rootToInterval IV Nat KeyMajor = _P4
    rootToInterval IV Sh KeyMajor = _a4
    rootToInterval V Fl KeyMajor = _d5
    rootToInterval V Nat KeyMajor = _P5
    rootToInterval V Sh KeyMajor = _a5
    rootToInterval VI Fl KeyMajor = _m6
    rootToInterval VI Nat KeyMajor = _M6
    rootToInterval VI Sh KeyMajor = _a6
    rootToInterval VII Fl KeyMajor = _m7
    rootToInterval VII Nat KeyMajor = _M7
    rootToInterval VII Sh KeyMajor = _a7
    rootToInterval I Fl KeyMinor = _d1
    rootToInterval I Nat KeyMinor = _P1
    rootToInterval I Sh KeyMinor = _a1
    rootToInterval II Fl KeyMinor = _m2
    rootToInterval II Nat KeyMinor = _M2
    rootToInterval II Sh KeyMinor = _a2
    rootToInterval III Fl KeyMinor = _m3
    rootToInterval III Nat KeyMinor = _M3
    rootToInterval III Sh KeyMinor = _a3
    rootToInterval IV Fl KeyMinor = _d4
    rootToInterval IV Nat KeyMinor = _P4
    rootToInterval IV Sh KeyMinor = _a4
    rootToInterval V Fl KeyMinor = _d5
    rootToInterval V Nat KeyMinor = _P5
    rootToInterval V Sh KeyMinor = _a5
    rootToInterval VI Fl KeyMinor = _d6
    rootToInterval VI Nat KeyMinor = _m6
    rootToInterval VI Sh KeyMinor = _M6
    rootToInterval VII Fl KeyMinor = _d7
    rootToInterval VII Nat KeyMinor = _m7
    rootToInterval VII Sh KeyMinor = _M7

