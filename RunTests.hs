import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Continue with more rules
 * Rule 91
 * Rule 92
 * Rule 99
 * &c.
* Define Chord data type
* Analyse harmony: Music -> [Chord]
 * Look at auxiliary note identification
* Granular composer: give it a grain of melody and it composes with it
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
