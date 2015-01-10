{-|
Module      : Numeral
Description : Definition and inspection of a chord as represented by a roman numeral
-}
module Numeral (
	Root(..),
	Inversion(..),
    _a, _b, _c, _d, _e, _f,
	Numeral,
    numeral,
	numeralToChord,
	rootNote,
    (^),
    (/),
    add
) where
import Key
import Note
import Interval
import Intervals
import Chord
import Data.List
import Data.Ord
import Prelude hiding ((^), (/))

-- |Root of a chord
data Root = I | II | III | IV | V | VI | VII deriving (Show, Eq, Ord, Enum)

-- |Chord inversion
data Inversion = First | Second | Third | Fourth | Fifth | Sixth deriving (Show, Eq, Ord, Enum)

-- |Handy short versions for inversions
_a, _b, _c, _d, _e, _f :: Inversion
_a = First
_b = Second
_c = Third
_d = Fourth
_e = Fifth
_f = Sixth

-- |Definition of a Roman Numeral representation of a chord
data Numeral = Numeral Root Alter [Interval] Inversion deriving (Show, Eq)

-- |Construct a Numeral value, making sure that the intervals are normalised and sorted
numeral :: Root -> Alter -> [Interval] -> Inversion -> Numeral
numeral r a is inv = Numeral r a (validate is) inv

validate :: [Interval] -> [Interval]
validate = sortBy (comparing chr) . map normalise

-- |Convert a numeral to a chord, given a specific Key
numeralToChord :: Key -> Numeral -> Chord
numeralToChord k h@(Numeral r a is inv) = chordFrom $ relocate $ rotate (fromEnum inv) notes
  where
    baseOctave = 3
    root = rootNote k baseOctave h
    notes = root : (map (applyInterval root) is)
    chordFrom ns = notesToChord (head ns) (tail ns)
    rotate n xs = take (length xs) (drop n (cycle xs))
    relocate ns = map (modifyOctave (3 - (Note.octave $ head ns))) ns

-- |Get the root note of a numeral, given a specific Key
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

-- |Set the inversion of a Numeral; eg _I^_b is the first inversion of the tonic. Eg, in C Major, it has the E in the bass
(^) :: Numeral -> Inversion -> Numeral
(^) (Numeral r a is _) inv = Numeral r a is inv

-- |Make the Numeral into a secondary; eg _V./V is a V of V secondary dominant
(/) :: Numeral -> Root -> Numeral
(/) (Numeral r a is i) root = Numeral (toEnum((fromEnum r + fromEnum root)`mod`7)) a is i

-- |Add a note into a numeral. Eg _I+_M9 would be a tonic triad with an added major ninth
add :: Numeral -> Interval -> Numeral
add (Numeral r a is i) int = Numeral r a (validate (is ++ [int])) i