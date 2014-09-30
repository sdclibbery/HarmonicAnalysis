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
  [ test []                                                               [Part "p" [c, d]]
  , test []                                                               [Part "p" [c, r, b]]
  , test [Error (Harmony 89) (Source "p" 0 1) "Dissonance Major7"]        [Part "p" [c, b]]
  , test [Error (Harmony 89) (Source "p" 1 2) "Dissonance Major7"]        [Part "p" [r, c, b, r]]
  , test []                                                               [Part "p" [beat B 3, beat C 4]]
  , test [Error (Harmony 89) (Source "p" 0 1) "Dissonance -Major7"]       [Part "p" [b, c]]
  , test [Error (Harmony 89) (Source "p" 1 2) "Dissonance Major7"]        [Part "p" [d, c, b]]
  , test [Error (Harmony 89) (Source "p2" 0 1) "Dissonance Major7"]       [Part "p" [c, d], Part "p2" [c, b]]
--  , test [Error (Harmony 89) (Source "p" 0 0) "Dissonance Major7"]        [Part "p" [Beat (Note $ C Nat 4) [B Nat 4]]]
--  , test [Error (Harmony 89) (Source "p" 0 0) "Dissonance Major7"]        [Part "p" [Beat Rest [C Nat 4, B Nat 4]]]
  ] where
    test e s = show s ~: e ~=? analyse s
