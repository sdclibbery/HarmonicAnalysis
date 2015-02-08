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


scale = [c, d, e, f, g, a, b, c', rw]

fixOctave oo ns = map (\(Play t (Note d a o)) -> (Play t (Note d a (o+oo)))) ns

returnToTonic :: Note -> [Event]
returnToTonic (Note C Nat o) = [hn $ Note C Nat o] ++ [rw]
returnToTonic (Note D Nat o) = [hn $ Note D Nat o] ++ fixOctave (o-4) [c] ++ [rw]
returnToTonic (Note E Nat o) = [hn $ Note E Nat o] ++ fixOctave (o-4) [d,c] ++ [rw]
returnToTonic (Note F Nat o) = [hn $ Note F Nat o] ++ fixOctave (o-4) [e,d,c] ++ [rw]
returnToTonic (Note G Nat o) = [hn $ Note G Nat o] ++ fixOctave (o-4) [a,b,c'] ++ [rw]
returnToTonic (Note A Nat o) = [hn $ Note A Nat o] ++ fixOctave (o-4) [b,c'] ++ [rw]
returnToTonic (Note B Nat o) = [hn $ Note B Nat o] ++ fixOctave (o-4) [c'] ++ [rw]

makeMusic :: Int -> [Note] -> [Event]
makeMusic i = concat . take num . insertEvery i scale . map returnToTonic
  where
    num = 35 *(min i 3)
    insertEvery i x xs = [x] ++ take i xs ++ insertEvery i x (drop i xs)


earTraining name i es = do
  g <- newStdGen
  createMidi ("eartraining/et_"++name++".midi") $ music [part g ns]
  where
    ns = map (\(Play _ n) -> n) es
    randomsChoice g xs = map (xs !!) $ randomRs (0, length xs - 1) g
    part g ns = makeMusic i $ randomsChoice g ns

main = do
  earTraining "1" 1 [c, c']
  earTraining "2" 1 [c, d, b, c']
  earTraining "3" 1 [c, d, e, a, b, c']
  earTraining "4" 1 [c, d, e, f, g, a, b, c']
  earTraining "5" 1 [b_, c, d, e, f, g, a, b, c', d']
  earTraining "6" 1 [a_, b_, c, d, e, f, g, a, b, c', d', e']
  earTraining "7" 1 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "8" 2 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "9" 3 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "10" 4 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "11" 5 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "12" 999999 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
