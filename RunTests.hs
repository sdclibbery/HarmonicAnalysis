import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Set up a random composer
 x Durations should be inverse powers of two
 x Able to compose until a given duration
 * Can compose multiple parts
  * Each part has an appropriate pitch range
* Modularise random composer
* Need to compose step by step
 * Pick shortest part to get next note
 * Pick appropriate note
 * Apply filter
* Can include rests
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
