import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Do a harmonic test next
 x Write rule properly
 x Write walkZippers properly...
 * Go back to applicative to support list of rules...
* Then back to Rule 90...
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
