import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Set up midi export and play
 * Duration
 * Rests
 * Instruments..?
* Set up a random composer
* Set up a random but filtered composer
* Continue with more rules
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
