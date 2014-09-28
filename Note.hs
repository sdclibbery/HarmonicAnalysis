module Note where

data Alter = DFl | Fl | Nat | Sh | DSh deriving (Eq, Ord, Show, Enum)

data Note = C Alter Int | D Alter Int | E Alter Int | F Alter Int | G Alter Int | A Alter Int | B Alter Int deriving (Eq, Show)

instance Ord Note where
  n < n' = absChromatic n < absChromatic n'

alteration :: Alter -> Int
alteration a = fromEnum a - 2

diatonic :: Note -> Int
diatonic (C _ _) = 0
diatonic (D _ _) = 1
diatonic (E _ _) = 2
diatonic (F _ _) = 3
diatonic (G _ _) = 4
diatonic (A _ _) = 5
diatonic (B _ _) = 6

chromatic :: Note -> Int
chromatic (C a _) = 0 + alteration a
chromatic (D a _) = 2 + alteration a
chromatic (E a _) = 4 + alteration a
chromatic (F a _) = 5 + alteration a
chromatic (G a _) = 7 + alteration a
chromatic (A a _) = 9 + alteration a
chromatic (B a _) = 11 + alteration a

octave :: Note -> Int
octave (C _ o) = o
octave (D _ o) = o
octave (E _ o) = o
octave (F _ o) = o
octave (G _ o) = o
octave (A _ o) = o
octave (B _ o) = o

absChromatic :: Note -> Int
absChromatic n = octave n * 12 + chromatic n

absDiatonic :: Note -> Int
absDiatonic n = octave n * 7 + diatonic n

