{-|
Module      : Key
Description : Definition and inspection of musical keys
-}
module Key (
	KeyQuality(..),
	Key(..)
) where
import Note

-- |Quality of a Key
data KeyQuality = KeyMajor | KeyMinor deriving (Show, Eq)

-- |Definition of a Key
data Key = Key Diatone Alter KeyQuality deriving (Show, Eq)
