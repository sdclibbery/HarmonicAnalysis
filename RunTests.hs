import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Do a harmonic test next
 * Need to switch to Report having a list of parts
 * Sort out data prep...
* Then back to Rule 90...
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
