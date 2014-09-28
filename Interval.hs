module Interval (
  Interval,
  interval,

  Quality(..),
  quality,

  step,
  consonant,
  dissonant,
  diminished,
  augmented
) where
import Note


data Interval = Interval {dia :: Int, chr :: Int, oct :: Int} deriving (Eq)

interval :: Note -> Note -> Interval
interval n n' = Interval (absDiatonic n' - absDiatonic n) (absChromatic n' - absChromatic n) (octave n' - octave n)

instance Show Interval where
  show i = sign ++ (show $ quality i) ++ (show $ abs(dia i) + 1)
    where
      sign = if dia i < 0 then "-" else ""


data Quality = Perfect | Major | Minor | Diminished | Augmented deriving (Eq, Show)

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
quality (Interval d c o) | o > 0 = quality (Interval d c (o-1))
quality _ = Diminished


step :: Note -> Note -> Bool
step n n' = abs(absDiatonic n' - absDiatonic n) == 1

dissonant :: Interval -> Bool
dissonant i = second i || seventh i || diminished i || augmented i
  where
    second i = abs(dia i) == 1 && abs(oct i) == 0
    seventh i = abs(dia i) == 6 && abs(oct i) == 0

consonant :: Interval -> Bool
consonant = not . dissonant

diminished :: Interval -> Bool
diminished i = quality i == Diminished

augmented :: Interval -> Bool
augmented i = quality i == Augmented
