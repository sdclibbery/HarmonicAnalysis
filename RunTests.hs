import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Filter random composition by applying Prout rules
* Continue with more rules
 * Haven't actually implemeted 96 properly yet!!
* Look at auxiliary note identification
* Granular composer: give it a grain of melody and it composes with it
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
