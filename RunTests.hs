import Test.HUnit
import qualified MelodyTests

{- TODO:
* Do a harmonic test next
 * Sort out data prep...
* Then back to Rule 90...
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  ]
