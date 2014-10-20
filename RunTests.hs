import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Need to compose step by step
 * Pick shortest part to get next note
 * Pick appropriate note
 * Apply filter
* Can include rests
* Modularise random composer
* Set up a random but filtered composer
 * Filter by applying Prout rules
 * Filter durations to hit bar lines..?
* Granular composer: give it a grain of melody and it composes with it
* Continue with more rules
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
