module HarmonyTests (
    tests
  ) where
import Test.HUnit
import Note
import Structure
import Report
import Harmony
import Compose

tests = TestLabel "Harmony" $ TestList
  [ testRuleH96
  , testRuleH99
  ]

music' = music . map (.>> 4)

testRuleH96 = TestLabel "ruleH96" $ TestList
    [ test []                                                                      $ music' [ [g, c, d], [e, d, e] ]
    , test []                                                                      $ music' [ [g, c, d], [e, c, e] ]
    , test []                                                                      $ music' [ [g, c, d], [e, d, d] ]
    , test [Error (Harmony 96) (Source [Bass, Treble] 1 3) "Consecutive unisons"]  $ music' [ [g, c, d], [e, c, d] ]
    , test []                                                                      $ music' [ [g, c, d], [e, c, r, d] ]
    , test [Error (Harmony 96) (Source [Bass, Treble] 2 4) "Consecutive unisons"]  $ music' [ [g, r, c, d, r], [e, r, c, d, r] ]
    , test []                                                                      $ music' [ [g, c, d], [e, b, c, d] ]
    , test [Error (Harmony 96) (Source [Bass, Treble] 2 4) "Consecutive unisons"]  $ music' [ [g, d, c, d, d], [e, b, c, d, b] ]
    , test []                                                                      $ music' [ [g, c, d], [e, d, e], [e, c, e] ]
    , test [Error (Harmony 96) (Source [Bass, Treble] 1 3) "Consecutive unisons"]  $ music' [ [g, c, d], [e, d, e], [e, c, d] ]
    , test [Error (Harmony 96) (Source [Bass, Treble] 1 3) "Consecutive octaves"]  $ music' [ [g, c, d], [e, c', d'] ]
    , test []                                                                      $ music' [ [g, c, c], [e, c, c] ]
    , test []                                                                      $ music' [ [g, c, c], [e, c', c'] ]
    , test []                                                                      $ music' [ [g, c, c], [e, c, c'] ]
    , test []                                                                      $ music' [ [g, c, c], [e, c, d], [e, d, c] ]
-- Parts that are nothing but consecutive octaves or unisons are allowed. However, its not worth complicating the code to handle this case at this stage
--     , test []                                                                      $ music' [ [c, d, e], [c, d, e] ]
--     , test []                                                                      $ music' [ [c, d, e], [c', d', e'] ]
    ] where
        test e m = show m ~: e ~=? analyse m

testRuleH99 = TestLabel "ruleH99" $ TestList
    [ test [Error (Harmony 99) (Source [Bass, Treble] 1 3) "Consecutive fifths"]     $ music' [ [g, c, d], [e, g, a] ]
    , test []                                                                        $ music' [ [g, c, c], [e, g, g] ]
    , test [Error (Harmony 99) (Source [Bass, Treble] 0 2) "Consecutive fifths"]     $ music' [ [c_, d_], [g, a] ]
    , test [Warning (Harmony 99) (Source [Bass, Treble] 0 2) "Consecutive fifths"]   $ music' [ [g__, c_], [d, g_] ]
    , test [Warning (Harmony 99) (Source [Alto, Treble] 0 2) "Consecutive fifths"]   $ music' [ [e_, e_], [c_, d_], [g, a] ]
    , test []                                                                        $ music' [ [c_, b__], [g, f] ]
    ] where
        test e m = show m ~: e ~=? analyse m

--   Diminished fifth followed by perfect is OK IFF neither part is the bass and the lower part rises a semitone
