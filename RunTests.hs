import Test.HUnit
import qualified MelodyTests

{- TODO:
* DataStructures
 * Can then extract single parts or pairs of parts
  * And in such a way that you can zipper through them 'in unison'
* Rule 89
  * ?? Should test incidentals?? What about rests??
* Do a harmonic test next
* Then back to Rule 90...
-}

main = runTestTT $ TestList
        [ MelodyTests.tests
        ]