import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
! Somehow knackered up the tests!
 - after just moving some definitions in the source! What?!
* Continue with more rules
 * Haven't actually implemeted 96 properly yet!!
  * Same rule forbids consecutive octaves as well as consecutive unisons
  * Repetition of the SAME octave/unison is OK
  * If one part leaps an octave, that is also OK
* Look at auxiliary note identification
* Granular composer: give it a grain of melody and it composes with it
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
