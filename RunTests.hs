import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Switch melody to use a walkZipper (with rest skipping) like the one in harmony
* Set up midi export and play
* Set up a random composer
* Set up a random but filtered composer
* Continue with more rules
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
