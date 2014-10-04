import Test.HUnit
import qualified MelodyTests

{- TODO:
* Rule 89
 * Refactor data prep
 * ?? Should test incidentals?? What about rests??
* Do a harmonic test next
 * Sort out data prep...
* Then back to Rule 90...
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  ]
