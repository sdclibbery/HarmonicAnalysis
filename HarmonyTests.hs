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

music ess = Music $ map makePart $ zip ["", "2", "3", "4"] ess
  where
    makePart (n, es) = Part ("p"++n) es

testRuleH96 = TestLabel "ruleH96" $ TestList
    [ test []                                                                      $ music [ [c, d], [d, e] ]
    , test []                                                                      $ music [ [c, d], [c, e] ]
    , test []                                                                      $ music [ [c, d], [d, d] ]
    , test [Error (Harmony 96) (Source ["p", "p2"] 0 2) "Consecutive unisons"]     $ music [ [c, d], [c, d] ]
    , test []                                                                      $ music [ [c, d], [c, r, d] ]
    , test [Error (Harmony 96) (Source ["p", "p2"] 1 3) "Consecutive unisons"]     $ music [ [r, c, d, r], [r, c, d, r] ]
    , test []                                                                      $ music [ [c, d], [b, c, d] ]
    , test [Error (Harmony 96) (Source ["p", "p2"] 1 3) "Consecutive unisons"]     $ music [ [d, c, d, d], [b, c, d, b] ]
    , test []                                                                      $ music [ [c, d], [d, e], [c, e] ]
    , test [Error (Harmony 96) (Source ["p", "p3"] 0 2) "Consecutive unisons"]     $ music [ [c, d], [d, e], [c, d] ]
    ] where
        test e m = show m ~: e ~=? analyse m
