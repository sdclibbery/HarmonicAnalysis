{-|
Module      : Structure
Description : Definition of musical structure for use in harmonic analysis
-}

module Structure (
  Event(..),
  Beat(..),
  Music(..)
) where
import Note

-- |Musical event
data Event = Rest | Note Note deriving (Eq, Show)

-- |Description of whats happening on one beat of the music
data Beat = Beat { event :: Event, incidental :: [Note] } deriving (Eq, Show)

-- |A musical part made up from a list of beats
type Part = [Beat]

-- |Entire music. A list of parts in ascending order from bass to treble
data Music = Music [Part] deriving (Eq, Show)

