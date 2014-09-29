{-|
Module      : Report
Description : Harmonic analysis report
-}

module Report (
  Ref(..),
  Source(..),
  Report(..)
) where

-- |A reference to the Prout books
data Ref = Harmony Int deriving (Eq, Show)

-- |A location in the source music: start and end beat
data Source = Source Int Int deriving (Eq, Show)

-- |A report of an issue in the music
data Report = Warning Ref Source String | Error Ref Source String deriving (Eq, Show)

