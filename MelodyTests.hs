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
  , testRuleH90
  ]

c, d, e, f, g, a, b, c', d', e', f', g', a', b', r :: Event
c = Note 1 $ C Nat 4
d = Note 1 $ D Nat 4
e = Note 1 $ E Nat 4
f = Note 1 $ F Nat 4
g = Note 1 $ G Nat 4
a = Note 1 $ A Nat 4
b = Note 1 $ B Nat 4
c' = Note 1 $ C Nat 5
d' = Note 1 $ D Nat 5
e' = Note 1 $ E Nat 5
f' = Note 1 $ F Nat 5
g' = Note 1 $ G Nat 5
a' = Note 1 $ A Nat 5
b' = Note 1 $ B Nat 5
r = Rest 1

music es = Music [Part "p" es]

testRuleH89 = TestLabel "ruleH89" $ TestList
  [ test []                                                                 $ music [c, d]
  , test []                                                                 $ music [c, r, b]
  , test [Error (Harmony 89) (Source ["p"] 0 2) "Dissonance Major7"]        $ music [c, b]
  , test [Error (Harmony 89) (Source ["p"] 1 3) "Dissonance Major7"]        $ music [r, c, b, r]
  , test []                                                                 $ music [Note 1 $ B Nat 3, Note 1 $ C Nat 4]
  , test [Error (Harmony 89) (Source ["p"] 0 2) "Dissonance -Major7"]       $ music [b, c]
  , test [Error (Harmony 89) (Source ["p"] 1 3) "Dissonance Major7"]        $ music [d, c, b]
  , test [Error (Harmony 89) (Source ["p2"] 0 2) "Dissonance Major7"]       $ Music [Part "p" [c, d], Part "p2" [c, b]]
  ] where
    test e s = show s ~: e ~=? analyse s

testRuleH90 = TestLabel "ruleH90" $ TestList
    [ test [Warning (Harmony 90) (Source ["p"] 0 2) "Diminished5"]                $ music [b, f']
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Unresolved Diminished5"]       $ music [b, f', c']
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Unresolved Diminished5"]       $ music [b, f', d']
    , test []                                                                     $ music [b, f', e']
    , test []                                                                     $ music [f', b, c']
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Unresolved -Diminished5"]      $ music [f', b, e']
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Outside Diminished5"]          $ music [b, f', a]
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Outside Diminished5"]          $ music [b, f', g']
    ] where
        test e s = show s ~: e ~=? analyse s
