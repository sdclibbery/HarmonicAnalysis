import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
! Broken: only chooses lowest or highest note!!
 - Test with single part
* Modularise random composer
* Support filtering while composing
 * Filter by applying Prout rules
 * Filter durations to hit bar lines..?
* Can include rests?
* Granular composer: give it a grain of melody and it composes with it
* Continue with more rules
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
