module MelodyTests (
    tests
  ) where
import Test.HUnit
import Note
import Structure
import Report
import Melody

tests = TestLabel "Melody" $ TestList
  [ testRuleH89
  ]

beat p o = Beat (Note $ p Nat o) []

c, d, e, b, r :: Beat
c = beat C 4
d = beat D 4
e = beat E 4
b = beat B 4
r = Beat Rest []

testRuleH89 = TestLabel "ruleH89" $ TestList
  [ test []                                                                $ Music [c, d]
  , test [Error (Harmony 89) (Source 0 1) "Dissonance Major7"]            $ Music [c, b]
  , test []                                                                $ Music [beat B 3, beat C 4]
  , test [Error (Harmony 89) (Source 0 1) "Dissonance -Major7"]           $ Music [b, c]
  , test [Error (Harmony 89) (Source 1 2) "Dissonance Major7"]            $ Music [d, c, b]
  ] where
    test e s = show s ~: e ~=? analyse s
