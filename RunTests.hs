import Test.HUnit
import qualified MelodyTests

{- TODO:
* Rule 89
  * ?? Should test incidentals?? What about rests??
* Do a harmonic test next
 * DataStructures
  * Input data is array of parts
  * Can then get single parts or pairs of parts
   * And in such a way that you can zipper through them 'in unison'
* Then back to Rule 90...
-}

main = runTestTT $ TestList
        [ MelodyTests.tests
        ]