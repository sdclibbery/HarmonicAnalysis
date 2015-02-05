import Structure
import Note
import Interval
import Intervals
import Notes
import Midi
import Key
import Keys
import Data.Ratio
import Data.List
import Data.Ord
import System.Random

-- Output relevant files to support ear training as suggested in http://www.miles.be/articles/7-ear-training-a-direct-and-logical-path


-- Scale then random note inside scale followed by journey back to tonic

-- Same but several of note+journey before repeating scale

-- Scale then random note which may be outside of scale followed by journey back to tonic

-- ...

scale = [c, d, e, f, g, a, b, c'] .<< 4


notes1 = concat $ replicate 5 $ scale ++ [rw]


main = do
	createMidi "eartraining1.midi" $ music [notes1]