module HarmonyTests (
    tests
  ) where
import Test.HUnit
import Note
import Structure
import Report
import Harmony

tests = TestLabel "Harmony" $ TestList
  [ testRuleH96
  ]

b, c, d, e, r :: Event
b = Note 1 $ B Nat 3
c = Note 1 $ C Nat 4
d = Note 1 $ D Nat 4
e = Note 1 $ E Nat 4
r = Rest 1

music es es' = Music [Part "p" es, Part "p2" es']

testRuleH96 = TestLabel "ruleH96" $ TestList
    [ {- test []                                                                $ music [c, d] [d, e]
    , test []                                                                $ music [c, d] [c, e]
    , test []                                                                $ music [c, d] [d, d]
    , test [Error (Harmony 96) (Source ["p", "p2"] 0 2) "Consecutive Unisons"]     $ music [c, d] [c, d]
    , test []                                                                $ music [c, d] [b, c, d]
    , test [Error (Harmony 96) (Source ["p", "p2"] 1 3) "Consecutive Unisons"]     $ music [d, c, d] [b, c, d] -}
    ] where
        test e m = show m ~: e ~=? analyse m
