import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Set up a random composer
 x Durations should be inverse powers of two
 * Able to compose until a given duration
 * Can compose multiple parts
  * Each part has an appropriate pitch range
 * Can include rests
* Modularise random composer
* Set up a random but filtered composer
* Granular composer: give it a grain of melody and it composes with it
* Continue with more rules
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
