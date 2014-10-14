import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
x Set up midi export and play
 x Duration
 x Rests
* Set up a random composer
* Set up a random but filtered composer
* Continue with more rules
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
