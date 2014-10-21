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
    let l = 10
    let (m,_) = until ((>= l).musicLength.fst) addEvent (Music [{- Part "bass" [], Part "tenor" [], Part "alto" [], -} Part "treble" []], g)
    putStrLn $ show m
    createMidi "test.midi" m

addEvent :: RandomGen g => (Music, g) -> (Music, g)
addEvent (Music ps, g) = (Music ps', g')
  where
    (i, sp) = findShortestPart ps
    sp' = addEventToPart sp e
    ps' = replace i sp' ps
    (e, g') = randomR (partRange $ name sp) g

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

partRange :: String -> (Event, Event)
partRange "treble" = (Play (1%8) $ Note C Nat 4, Play (1%2) $ Note C Nat 6)
partRange "alto"   = (Play (1%8) $ Note G Nat 3, Play (1%2) $ Note F Nat 5)
partRange "tenor"  = (Play (1%8) $ Note C Nat 3, Play (1%2) $ Note C Nat 5)
partRange "bass"   = (Play (1%8) $ Note E Nat 2, Play (1%2) $ Note E Nat 4)

instance Random Note where
  random g = randomR (Note C Fl 0, Note B Sh 7) g
  randomR (Note dlo alo olo, Note dhi ahi ohi) g = (Note (toEnum d) (toEnum a) o, g''')
    where
      (d, g') = randomR (fromEnum dlo, fromEnum dhi) g
      (a, g'') = randomR (fromEnum alo, fromEnum ahi) g'
      (o, g''') = randomR (olo, ohi) g''

instance Random Event where
  random g = randomR (Play (1%8) $ Note C Fl 0, Play (1%2) $ Note B Sh 7) g
  randomR (Play dlo nlo, Play dhi nhi) g = (Play d n, g'')
    where
      (d, g') = randomRDuration (dlo, dhi) g
      (n, g'') = randomR (nlo, nhi) g'

randomRDuration :: RandomGen g => (Time, Time) -> g -> (Time, g)
randomRDuration (lo, hi) g = (1 % (pow2 n), g)
  where
    (n, g') = randomR (denominator lo, denominator hi) g
    pow2 n = 2 ^ (round $ logBase 2 $ fromIntegral n)
