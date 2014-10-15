import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Split Midi export into own file
* Set up a random composer
 * Durations should be inverse powers of two, or dotted versions only
 * Can include rests
 * Able to compose multiple until a given duration
  * Each part has an appropriate pitch range
* Set up a random but filtered composer
* Continue with more rules
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
