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

-- |Entire music
data Music = Music [Beat] deriving (Eq, Show)

