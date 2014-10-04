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

c, d, e, b, r :: Event
c = Note 1 $ C Nat 4
d = Note 1 $ D Nat 4
e = Note 1 $ E Nat 4
b = Note 1 $ B Nat 4
r = Rest 1

music es = Music [Part "p" es]

testRuleH89 = TestLabel "ruleH89" $ TestList
  [ test []                                                               $ music [c, d]
  , test []                                                               $ music [c, r, b]
  , test [Error (Harmony 89) (Source "p" 0 2) "Dissonance Major7"]        $ music [c, b]
  , test [Error (Harmony 89) (Source "p" 1 3) "Dissonance Major7"]        $ music [r, c, b, r]
  , test []                                                               $ music [Note 1 $ B Nat 3, Note 1 $ C Nat 4]
  , test [Error (Harmony 89) (Source "p" 0 2) "Dissonance -Major7"]       $ music [b, c]
  , test [Error (Harmony 89) (Source "p" 1 3) "Dissonance Major7"]        $ music [d, c, b]
--  , test [Error (Harmony 89) (Source "p2" 0 2) "Dissonance Major7"]       $ music [c, d], Part "p2" [c, b]
--  , test [Error (Harmony 89) (Source "p" 0 0) "Dissonance Major7"]        $ music [Beat (Note $ C Nat 4) [B Nat 4]]
--  , test [Error (Harmony 89) (Source "p" 0 0) "Dissonance Major7"]        $ music [Beat Rest [C Nat 4, B Nat 4]]
  ] where
    test e s = show s ~: e ~=? analyse s
