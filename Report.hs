{-|
Module      : Report
Description : Harmonic analysis report
-}

module Report (
  Ref(..),
  Source(..),
  Message,
  Report(..),
  startTime
) where
import Structure

-- |A report of an issue in the music
data Report = Warning Ref Source Message | Error Ref Source Message deriving (Eq, Show)

-- |A reference to the Prout books
data Ref = Harmony Int deriving (Eq, Show)

-- |A location in the source music: start and end beat
data Source = Source { parts :: [PartName], start :: Time, end :: Time } deriving (Eq, Show)

-- |The message report
type Message = String

-- |Get the start time of a report
startTime :: Report -> Time
startTime (Warning _ s _) = start s
startTime (Error _ s _) = start s
