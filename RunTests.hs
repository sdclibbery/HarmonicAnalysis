import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
x Harmonic analysis needs to handle rests
* Then back to Rule 90...
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
