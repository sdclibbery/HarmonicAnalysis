module Melody where
import Data.Maybe



-- Notes

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



-- Intervals

data Interval = Interval {dia :: Int, chr :: Int, oct :: Int} deriving (Eq)

data Quality = Perfect | Major | Minor | Diminished | Augmented deriving (Eq, Show)

instance Show Interval where
  show i = sign ++ (show $ quality i) ++ (show $ abs(dia i) + 1)
    where
      sign = if dia i < 0 then "-" else ""

interval :: Note -> Note -> Interval
interval n n' = Interval (absDiatonic n' - absDiatonic n) (absChromatic n' - absChromatic n) (octave n' - octave n)

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

isUnison i = abs(dia i) == 0 && abs(oct i) == 0
isSecond i = abs(dia i) == 1 && abs(oct i) == 0
isThird i = abs(dia i) == 2 && abs(oct i) == 0
isFourth i = abs(dia i) == 3 && abs(oct i) == 0
isFifth i = abs(dia i) == 4 && abs(oct i) == 0
isSixth i = abs(dia i) == 5 && abs(oct i) == 0
isSeventh i = abs(dia i) == 6 && abs(oct i) == 0
isOctave i = abs(dia i) == 7 && abs(oct i) == 0
isNinth i = abs(dia i) == 1 && abs(oct i) == 1
isEleventh i = abs(dia i) == 3 && abs(oct i) == 1
isThirteenth i = abs(dia i) == 5 && abs(oct i) == 1

step :: Note -> Note -> Bool
step n n' = (abs diff) == 1
  where
    diff = absDiatonic n' - absDiatonic n

dissonant :: Interval -> Bool
dissonant i = isSecond i || isSeventh i || diminished i || augmented i

consonant :: Interval -> Bool
consonant = not . dissonant

diminished :: Interval -> Bool
diminished i = quality i == Diminished

augmented :: Interval -> Bool
augmented i = quality i == Augmented



-- Structure

data Event = Rest | Note Note deriving (Eq, Show)

data Beat = Beat { event :: Event, incidental :: [Note] } deriving (Eq, Show)

data Music = Music [Beat] deriving (Eq, Show)



-- Results

data Ref = Harmony Int deriving (Eq, Show)

data Context = Context Int Int deriving (Eq, Show)

data Result = Warning Ref Context String | Error Ref Context String deriving (Eq, Show)





analyse :: Music -> [Result]
analyse (Music bs) = catMaybes $ map rule89 $ zip3 [0..] bs $ drop 1 bs

rule89 :: (Int, Beat, Beat) -> Maybe Result
rule89 (i, Beat Rest _, Beat _ _) = Nothing
rule89 (i, Beat _ _, Beat Rest _) = Nothing
rule89 (i, Beat (Note n) _, Beat (Note n') _) | step n n' = Nothing
rule89 (i, Beat (Note n) _, Beat (Note n') _) | consonant (interval n n') = Nothing
rule89 (i, Beat (Note n) _, Beat (Note n') _) | diminished (interval n n') = Nothing -- Leave for next rule
rule89 (i, Beat (Note n) _, Beat (Note n') _) | augmented (interval n n') = Nothing -- Leave for next rule
rule89 (i, Beat (Note n) _, Beat (Note n') _) = Just $ Error (Harmony 89) (Context i (i+1)) $ "Dissonance " ++ show (interval n n')


