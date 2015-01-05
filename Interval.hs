{-|
Module      : Interval
Description : Definition and inspection of musical intervals between notes
-}

module Interval (
  Interval,
  dia, chr, oct,
  interval,

  Quality(..),
  quality,

  normalise,
  applyInterval,

  consonant,
  dissonant,
  diminished,
  minor,
  augmented,
  perfect,

  unison,
  second,
  step,
  fifth,
  Interval.octave,
  large
) where
import Note

-- |Musical interval: interval between two notes
data Interval = Interval {dia :: Int, chr :: Int, oct :: Int} deriving (Eq)

-- |Create an interval from two notes
interval :: Note -> Note -> Interval
interval n n' = Interval d c o
  where
    d = absDiatonic n' - absDiatonic n
    c = absChromatic n' - absChromatic n
    o = if c < 0 then 1 + (c `div` 12) else c `div` 12

instance Show Interval where
  show i = sign ++ (show $ quality i) ++ (show $ abs(dia i) + 1) ++ octave
    where
      sign = if dia i < 0 then "-" else ""
      octave = if oct i /= 0 then "^"++(show $ oct i) else ""

-- |Interval Quality
data Quality = Perfect | Major | Minor | Diminished | Augmented deriving (Eq, Show)

-- |Get the quality for a given interval
quality :: Interval -> Quality
quality (Interval 0 (-1) 0) = Diminished
quality (Interval 0 0 0) = Perfect
quality (Interval 0 1 0) = Augmented
quality (Interval 1 0 0) = Diminished
quality (Interval 1 1 0) = Minor
quality (Interval 1 2 0) = Major
quality (Interval 1 3 0) = Augmented
quality (Interval 2 2 0) = Diminished
quality (Interval 2 3 0) = Minor
quality (Interval 2 4 0) = Major
quality (Interval 2 5 0) = Augmented
quality (Interval 3 4 0) = Diminished
quality (Interval 3 5 0) = Perfect
quality (Interval 3 6 0) = Augmented
quality (Interval 4 6 0) = Diminished
quality (Interval 4 7 0) = Perfect
quality (Interval 4 8 0) = Augmented
quality (Interval 5 7 0) = Diminished
quality (Interval 5 8 0) = Minor
quality (Interval 5 9 0) = Major
quality (Interval 5 10 0) = Augmented
quality (Interval 6 9 0) = Diminished
quality (Interval 6 10 0) = Minor
quality (Interval 6 11 0) = Major
quality (Interval 6 12 0) = Augmented
quality (Interval d c o) | d < 0 = quality (Interval (-d) (-c) (-o))
quality (Interval d c o) | d > 6 || o > 0 = quality (Interval (d `mod` 7) (c `mod` 12) 0)
quality (Interval d c o) = error $ "quality: Invalid interval: dia: " ++ (show d) ++ " chr: " ++ (show c) ++ " oct: " ++ (show o)

-- |Normalise an interval so it is within an octave
normalise :: Interval -> Interval
normalise (Interval d c o) | d < 0 = normalise $ Interval (d+7) (c+12) (o+1)
normalise (Interval d c o) = Interval (d `mod` 7) (c `mod` 12) 0

-- |Apply an Interval to a Note: offset the note by the given interval
applyInterval :: Note -> Interval -> Note
applyInterval n@(Note d a o) (Interval di ci oi) = Note d' a' o'
  where
    dd = fromEnum d + di
    chromaticDiff = absChromatic (Note d' Nat o') - absChromatic n
    d' = toEnum $ dd `mod` 7
    a' = toEnum $ fromEnum Nat + ci - chromaticDiff
    o' = o + oi + (dd `div` 7)

-- |Check if an interval is dissonant
dissonant :: Interval -> Bool
dissonant i = second i || seventh i || diminished i || augmented i
  where
    second i = abs(dia i) == 1 && abs(oct i) == 0
    seventh i = abs(dia i) == 6 && abs(oct i) == 0

-- |Check if an interval is consonant
consonant :: Interval -> Bool
consonant = not . dissonant

-- |Check if an interval is diminished
diminished :: Interval -> Bool
diminished i = quality i == Diminished

-- |Check if an interval is minor
minor :: Interval -> Bool
minor i = quality i == Minor

-- |Check if an interval is augmented
augmented :: Interval -> Bool
augmented i = quality i == Augmented

-- |Check if an interval is perfect
perfect :: Interval -> Bool
perfect i = quality i == Perfect

-- |Check if an interval is a unison
unison :: Interval -> Bool
unison i = abs(dia i) == 0

-- |Check if an interval is a second
second :: Interval -> Bool
second i = abs(dia i) `mod` 7 == 1

-- |Check if an interval is a step
step :: Interval -> Bool
step i = abs(dia i) == 1

-- |Check if an interval is a octave
octave :: Interval -> Bool
octave i = (abs(dia i) `mod` 7 == 0) && (not $ unison i)

-- |Check if an interval is a fifth
fifth :: Interval -> Bool
fifth i = abs(dia i) `mod` 7 == 4

-- |Check if an interval is large (a seventh or more)
large :: Interval -> Bool
large i = abs(dia i) > 5
