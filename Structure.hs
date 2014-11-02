{-|
Module      : Structure
Description : Definition of musical structure for use in harmonic analysis
-}

module Structure (
  Time(..),
  PartName(..),
  Event(..),
  Part(..),
  Music(..)
) where
import Note
import Data.List
import Data.Ord

-- |Time type
type Time = Rational

-- |Part name
data PartName = Bass | Tenor | Alto | Treble deriving (Eq, Show)

-- |One note or rest in a part
data Event = Rest Time | Play Time Note deriving (Eq, Show)

-- |List of notes and rests in sequence
data Part = Part { name :: PartName, events :: [Event] } deriving (Eq, Show)

-- |Entire music made up of a list of parts in order from bass to treble
data Music = Music [Part] deriving (Eq, Show)
