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

scale = [c, d, e, f, g, a, b, c']

instance Random Note where
  random g = randomR (Note C Nat 4, Note C Nat 5) g
  randomR (nlo, nhi) g = (Note (toEnum d) Nat o, g')
    where
      drange = (absDiatonic nlo, absDiatonic nhi)
      (ad, g') = randomR drange g
      o = ad `div` 7
      d = ad `mod` 7

returnToTonic :: Note -> [Event]
returnToTonic (Note C Nat o) = [hn $ Note C Nat o]
returnToTonic (Note D Nat o) = [hn $ Note D Nat o] ++ fixOctave (o-4) [c]
returnToTonic (Note E Nat o) = [hn $ Note E Nat o] ++ fixOctave (o-4) [d,c]
returnToTonic (Note F Nat o) = [hn $ Note F Nat o] ++ fixOctave (o-4) [e,d,c]
returnToTonic (Note G Nat o) = [hn $ Note G Nat o] ++ fixOctave (o-4) [a,b,c']
returnToTonic (Note A Nat o) = [hn $ Note A Nat o] ++ fixOctave (o-4) [b,c']
returnToTonic (Note B Nat o) = [hn $ Note B Nat o] ++ fixOctave (o-4) [c']

fixOctave oo ns = map (\(Play t (Note d a o)) -> (Play t (Note d a (o+oo)))) ns

makeMusic :: [Note] -> [Event]
makeMusic ns = concat $ take 50 $ map (\n -> scale ++ [rw] ++ returnToTonic n ++ [rw]) ns

randomsChoice :: RandomGen g => g -> [a] -> [a]
randomsChoice g xs = map (xs !!) $ randomRs (0, length xs - 1) g

earTraining name es = do
  g <- newStdGen
  createMidi ("eartraining/et_"++name++".midi") $ music [makeMusic $ randomsChoice g ns]
  where
    ns = map toNote es
    toNote (Play _ n) = n

main = do
  earTraining "1" [c, c']
  earTraining "2" [c, d, b, c']
  earTraining "3" [c, d, e, a, b, c']
  earTraining "4" [c, d, e, f, g, a, b, c']
  earTraining "5" [b_, c, d, e, f, g, a, b, c', d']
  earTraining "6" [a, b_, c, d, e, f, g, a, b, c', d', e']
