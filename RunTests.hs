import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
x Switch away from having indivdual diatone constructors in Note
* Set up a random composer
* Set up a random but filtered composer
* Continue with more rules
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
