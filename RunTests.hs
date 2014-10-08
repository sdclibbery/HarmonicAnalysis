import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Then back to Rule 90...
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
