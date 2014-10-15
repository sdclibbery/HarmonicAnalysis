import Data.List
import Data.Ratio
import Structure
import Note
import Compose
import Midi
import System.Random


main = do
    seed  <- newStdGen
    let es = randomEvents 10 seed
    let m = music [ es ]
    putStrLn $ show m
--    createMidi "test.midi" m


instance Random Note where
  random g = randomR (Note C Fl 0, Note B Sh 7) g
  randomR (Note dlo alo olo, Note dhi ahi ohi) g = (Note (toEnum d) (toEnum a) o, g''')
    where
      (d, g') = randomR (fromEnum dlo, fromEnum dhi) g
      (a, g'') = randomR (fromEnum alo, fromEnum ahi) g'
      (o, g''') = randomR (olo, ohi) g''

instance Random Event where
  random g = randomR (Play (1%16) $ Note C Fl 0, Play 1 $ Note B Sh 7) g
  randomR (Play dlo nlo, Play dhi nhi) g = (Play (1%d) n, g'')
    where
      (d, g') = randomR (denominator dlo, denominator dhi) g
      (n, g'') = randomR (nlo, nhi) g'

randomEvents :: Int -> StdGen -> [Event]
randomEvents n = take n . unfoldr (Just . random)


