import Data.List
import Data.Ratio
import Structure
import Note
import Compose
import Midi
import Data.Ord
import System.Random


main = do
    g <- newStdGen
    let l = 20
    let (m,_) = until ((>= l).musicLength.fst) addEvent (Music [Part "bass" [], Part "tenor" [], Part "alto" [], Part "treble" []], g)
    putStrLn $ show m
    createMidi "test.midi" m

addEvent :: RandomGen g => (Music, g) -> (Music, g)
addEvent (Music ps, g) = (Music ps', g'')
  where
    (i, sp) = findShortestPart ps
    ps' = replace i sp' ps
    sp' = addEventToPart sp $ Play d n
    (n, g') = randomR (partRange $ name sp) g
    (d, g'') = randomRDuration ((1%8), (1%2)) g'

findShortestPart :: [Part] -> (Int, Part)
findShortestPart ps = head $ sortBy (comparing (partLength.snd)) $ zip [0..] ps

addEventToPart :: Part -> Event -> Part
addEventToPart (Part n es) e = Part n (es ++ [e])

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

musicLength :: Music -> Time
musicLength (Music ps) = foldr ((+).partLength) 0 ps

partLength :: Part -> Time
partLength (Part _ es) = foldr ((+).eventLength) 0 es

eventLength :: Event -> Time
eventLength (Rest d) = d
eventLength (Play d _) = d

partRange :: String -> (Note, Note)
partRange "treble" = (Note C Nat 4, Note C Nat 6)
partRange "alto"   = (Note G Nat 3, Note F Nat 5)
partRange "tenor"  = (Note C Nat 3, Note C Nat 5)
partRange "bass"   = (Note E Nat 2, Note E Nat 4)

instance Random Note where
  random g = randomR (Note C Fl 0, Note B Sh 7) g
  randomR (nlo, nhi) g = (Note (toEnum d) (toEnum a) o, g')
    where
      crange = (absChromatic nlo, absChromatic nhi)
      (ac, g') = randomR crange g
      c = ac `mod` 12
      d = (c * 6) `div` 12 -- Not really right
      a = 1 + (c * 2) `div` 12 -- Not right at all
      o = ac `div` 12

randomRDuration :: RandomGen g => (Time, Time) -> g -> (Time, g)
randomRDuration (lo, hi) g = (1 % (pow2 n), g)
  where
    (n, g') = randomR (denominator lo, denominator hi) g
    pow2 n = 2 ^ (round $ logBase 2 $ fromIntegral n)
