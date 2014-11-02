import Test.HUnit
import qualified MelodyTests
import qualified HarmonyTests

{- TODO:
* Support for progressive analysis: only analyse starting two bars from the end
* Continue with more rules
 * Rule 101
 * Rule 102: hidden fifths/octaves
 * &c.
* Define Chord data type
* Analyse harmony: Music -> [Chord] and Music -> [Key]
 * Look at auxiliary note identification
* Rule 99: consecutive fifths are only warning if going from dominant to tonic
* Rules relating to Chord and Key progressions
* Chord composer: give it a key and it picks random chords and composes against them
* Key composer: creates random key sequence and composes to it
* Granular composer: give it a grain of melody and it composes with it
-}

main = runTestTT $ TestList
  [ MelodyTests.tests
  , HarmonyTests.tests
  ]
