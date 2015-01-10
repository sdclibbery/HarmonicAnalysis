{-|
Module      : Numeral
Description : Definition and inspection of a chord as represented by a roman numeral
-}
module Numeral (
	Root(..),
	Inversion(..),
	Numeral,
    numeral,
	numeralToChord,
	rootNote,
    (.^), (./), add,
    (#), (♭),
    _a, _b, _c, _d, _e, _f
) where
import Key
import Note
import Interval
import Intervals
import Chord
import Data.List
import Data.Ord

-- |Root of a chord
data Root = FlI | I | ShI | FlII | II | ShII | FlIII | III | ShIII | FlIV | IV | ShIV | FlV | V | ShV | FlVI | VI | ShVI | FlVII | VII | ShVII deriving (Show, Eq, Ord, Enum)

-- |Chord inversion
data Inversion = First | Second | Third | Fourth | Fifth | Sixth deriving (Show, Eq, Ord, Enum)

-- |Definition of a Roman Numeral representation of a chord
data Numeral = Numeral Root [Interval] Inversion deriving (Show, Eq)

-- |Construct a Numeral value, making sure that the intervals are normalised and sorted
numeral :: Root -> [Interval] -> Inversion -> Numeral
numeral r is inv = Numeral r (validate is) inv

-- |Convert a numeral to a chord, given a specific Key
numeralToChord :: Key -> Numeral -> Chord
numeralToChord k h@(Numeral r is inv) = chordFrom $ relocate $ rotate (fromEnum inv) notes
  where
    baseOctave = 3
    root = rootNote k baseOctave h
    notes = root : (map (applyInterval root) is)
    chordFrom ns = notesToChord (head ns) (tail ns)
    rotate n xs = take (length xs) (drop n (cycle xs))
    relocate ns = map (modifyOctave (3 - (Note.octave $ head ns))) ns

-- |Get the root note of a numeral, given a specific Key
rootNote :: Key -> Int -> Numeral -> Note
rootNote (Key d a q) o (Numeral r _ _) = applyInterval (Note d a o) (rootToInterval r q)
  where
    rootToInterval FlI KeyMajor = _d1
    rootToInterval I KeyMajor = _P1
    rootToInterval ShI KeyMajor = _a1
    rootToInterval FlII KeyMajor = _m2
    rootToInterval II KeyMajor = _M2
    rootToInterval ShII KeyMajor = _a2
    rootToInterval FlIII KeyMajor = _m3
    rootToInterval III KeyMajor = _M3
    rootToInterval ShIII KeyMajor = _a3
    rootToInterval FlIV KeyMajor = _d4
    rootToInterval IV KeyMajor = _P4
    rootToInterval ShIV KeyMajor = _a4
    rootToInterval FlV KeyMajor = _d5
    rootToInterval V KeyMajor = _P5
    rootToInterval ShV KeyMajor = _a5
    rootToInterval FlVI KeyMajor = _m6
    rootToInterval VI KeyMajor = _M6
    rootToInterval ShVI KeyMajor = _a6
    rootToInterval FlVII KeyMajor = _m7
    rootToInterval VII KeyMajor = _M7
    rootToInterval ShVII KeyMajor = _a7
    rootToInterval FlI KeyMinor = _d1
    rootToInterval I KeyMinor = _P1
    rootToInterval ShI KeyMinor = _a1
    rootToInterval FlII KeyMinor = _m2
    rootToInterval II KeyMinor = _M2
    rootToInterval ShII KeyMinor = _a2
    rootToInterval FlIII KeyMinor = _m3
    rootToInterval III KeyMinor = _M3
    rootToInterval ShIII KeyMinor = _a3
    rootToInterval FlIV KeyMinor = _d4
    rootToInterval IV KeyMinor = _P4
    rootToInterval ShIV KeyMinor = _a4
    rootToInterval FlV KeyMinor = _d5
    rootToInterval V KeyMinor = _P5
    rootToInterval ShV KeyMinor = _a5
    rootToInterval FlVI KeyMinor = _d6
    rootToInterval VI KeyMinor = _m6
    rootToInterval ShVI KeyMinor = _M6
    rootToInterval FlVII KeyMinor = _d7
    rootToInterval VII KeyMinor = _m7
    rootToInterval ShVII KeyMinor = _M7

-- |Set the inversion of a Numeral; eg _I^_b is the first inversion of the tonic. Eg, in C Major, it has the E in the bass
(.^) :: Numeral -> Inversion -> Numeral
(.^) (Numeral r is _) inv = Numeral r is inv

-- |Make the Numeral into a secondary; eg _V./V is a V of V secondary dominant
(./) :: Numeral -> Root -> Numeral
(./) (Numeral r is i) root = Numeral (toEnum((fromEnum r + fromEnum root)`mod`7)) is i

-- |Add a note into a numeral. Eg _I+_M9 would be a tonic triad with an added major ninth
add :: Numeral -> Interval -> Numeral
add (Numeral r is i) int = Numeral r (validate (is ++ [int])) i

-- |Sharpen a root
(#) :: Root -> Root
(#) I = ShI
(#) II = ShII
(#) III = ShIII
(#) IV = ShIV
(#) V = ShV
(#) VI = ShVI
(#) VII = ShVII

-- |Flatten a root
(♭) :: Root -> Root
(♭) I = FlI
(♭) II = FlII
(♭) III = FlIII
(♭) IV = FlIV
(♭) V = FlV
(♭) VI = FlVI
(♭) VII = FlVII

-- |Handy short versions for inversions
_a, _b, _c, _d, _e, _f :: Inversion
_a = First
_b = Second
_c = Third
_d = Fourth
_e = Fifth
_f = Sixth

-- Helper to validate (normalise and sort) a list of intervals
validate :: [Interval] -> [Interval]
validate = sortBy (comparing chr) . map normalise

