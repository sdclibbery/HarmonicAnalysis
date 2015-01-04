{-|
Module      : Note
Description : Definition and inspection of musical notes
-}

module Note (
  Alter(..),
  Diatone(..),
  Note(..),
  diatonic,
  chromatic,
  octave,
  modifyOctave,
  absChromatic,
  absDiatonic
) where


-- |Note alteration: sharpening or flattening
data Alter = DFl | Fl | Nat | Sh | DSh deriving (Eq, Ord, Show, Enum)

-- |Diatone
data Diatone = C | D | E | F | G | A | B deriving (Show, Ord, Eq, Enum, Bounded)

-- |Musical Note: Base pitch class, alteration, and octave
data Note = Note Diatone Alter Int deriving (Show)

instance Eq Note where
  n == n' = absChromatic n == absChromatic n'

instance Ord Note where
  n `compare` n' = absChromatic n `compare` absChromatic n'

alteration :: Alter -> Int
alteration a = fromEnum a - 2

-- |Get the diatonic index for a note; C is 0, D is 1, ... B is 6
diatonic :: Note -> Int
diatonic (Note d _ _) = fromEnum d

-- |Get the chromatic index for a note; C Nat is 0, C Sh is 1, ... B is 11
chromatic :: Note -> Int
chromatic (Note C a _) = 0 + alteration a
chromatic (Note D a _) = 2 + alteration a
chromatic (Note E a _) = 4 + alteration a
chromatic (Note F a _) = 5 + alteration a
chromatic (Note G a _) = 7 + alteration a
chromatic (Note A a _) = 9 + alteration a
chromatic (Note B a _) = 11 + alteration a

-- |Get the octave for a note
octave :: Note -> Int
octave (Note _ _ o) = o

-- |Modify the octave for a note
modifyOctave :: Int -> Note -> Note
modifyOctave od (Note d a o) = Note d a (o + od)

-- |Get the number of semitones from (C Nat 0)
absChromatic :: Note -> Int
absChromatic n = octave n * 12 + chromatic n

-- |Get the number of diatones from (C Nat 0)
absDiatonic :: Note -> Int
absDiatonic n = octave n * 7 + diatonic n

