module MelodyTests (
    tests
  ) where
import Test.HUnit
import Note
import Structure
import Report
import Melody
import Compose

tests = TestLabel "Melody" $ TestList
  [ testRuleH89
  , testRuleH90
  , testRuleH91
  ]

music' = music . map (.>> 4)

testRuleH89 = TestLabel "ruleH89" $ TestList
  [ test []                                                                 $ music' [ [c, d] ]
  , test []                                                                 $ music' [ [c, r, b] ]
  , test [Error (Harmony 89) (Source ["p"] 0 2) "Dissonance Major7"]        $ music' [ [c, b] ]
  , test [Error (Harmony 89) (Source ["p"] 1 3) "Dissonance Major7"]        $ music' [ [r, c, b, r] ]
  , test []                                                                 $ music' [ [b_, c] ]
  , test [Error (Harmony 89) (Source ["p"] 0 2) "Dissonance -Major7"]       $ music' [ [b, c] ]
  , test [Error (Harmony 89) (Source ["p"] 1 3) "Dissonance Major7"]        $ music' [ [d, c, b] ]
  , test [Error (Harmony 89) (Source ["p2"] 0 2) "Dissonance Major7"]       $ music' [ [c, d], [c, b] ]
  ] where
    test e s = show s ~: e ~=? analyse s

testRuleH90 = TestLabel "ruleH90" $ TestList
    [ test [Warning (Harmony 90) (Source ["p"] 0 2) "Diminished5"]                $ music' [ [b, f'] ]
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Unresolved Diminished5"]       $ music' [ [b, f', c'] ]
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Unresolved Diminished5"]       $ music' [ [b, f', d'] ]
    , test []                                                                     $ music' [ [b, f', e'] ]
    , test []                                                                     $ music' [ [f', b, c'] ]
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Unresolved -Diminished5"]      $ music' [ [f', b, e'] ]
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Outside Diminished5"]          $ music' [ [b, f', a] ]
    , test [Error (Harmony 90) (Source ["p"] 0 2) "Outside Diminished5"]          $ music' [ [b, f', g'] ]
    ] where
        test e s = show s ~: e ~=? analyse s

testRuleH91 = TestLabel "ruleH91" $ TestList
  [ test [Error (Harmony 91) (Source ["p"] 0 2) "Augmented4"]                $ music' [ [f, b] ]
  , test []                                                                    $ music' [ [gf, a] ]
  ] where
    test e s = show s ~: e ~=? analyse s

