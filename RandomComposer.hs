import Data.List
import Data.Ratio
import Structure
import Note
import Compose
import Midi
import System.Random


main = do
    seed  <- newStdGen
    let es = randomRMelody 10 seed
    let m = music [ es ]
    putStrLn $ show m
    createMidi "test.midi" m


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

randomRMelody :: Time -> StdGen -> [Event]
randomRMelody n g = fst $ until (\(es,_) -> length es >= n) composeNote ([], g)
  where
    length es = foldr (\e l -> duration e + l) 0 es
    duration (Rest d) = d
    duration (Play d _) = d
    composeNote (es,g) = let (e, g') = random g in (es ++ [e], g')


