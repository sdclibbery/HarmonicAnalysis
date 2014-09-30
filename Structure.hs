{-|
Module      : Structure
Description : Definition of musical structure for use in harmonic analysis
-}

module Structure (
  Event(..),
  Beat(..),
  PartName,
  Part(..),
  Music(..)
) where
import Note

-- |Musical event
data Event = Rest | Note Note deriving (Eq, Show)

-- |Description of whats happening on one beat of the music
data Beat = Beat { event :: Event, incidental :: [Note] } deriving (Eq, Show)

-- |Part name
type PartName = String

-- |A musical part made up from a list of beats
data Part = Part { name :: PartName, beats :: [Beat] } deriving (Eq, Show)

-- |Entire music. A list of parts in ascending order from bass to treble
type Music = [Part]

