import Structure
import Note
import Compose
import Midi

-- C Major prelude - Book one, well tempered clavier
chords = [
    [c, e, g, c', e'],    -- C
    [c, d, a, d', f'],    -- Dm7
    [b_, d, g, b, f'],    -- G7
    [c, e, g, c', e'],    -- C
    [c, e, a, e', a'],    -- Am
    [c, d, fs, a, d'],    -- D7
    [b_, d, g, d', g'],   -- G
    [a_, c, e, g, c'],    -- Am7
    [d_, a_, d, fs, c'],  -- D7
    [g_, b_, d, g, b],    -- G
    [g_, bf_, e, g, cs'], -- Gdim9
    [f_, a_, d, a, d'],   -- Dm
    [f_, af_, d, f, b],   -- Fdim9
    [e_, g_, c, g, c'],   -- C
    [e_, f_, a_, c, f],    -- Fmaj7
    [d_, f_, a_, c, f],    -- Dm7
    [g__, d_, g_, b_, f], -- G7
    [c_, e_, g_, c, e],   -- C
    [c_, g_, bf_, c, e],  -- C7
    [f__, f_, a_, c, e],  -- Fmaj7
    [fs__, c_, a_, c, ef],-- F#dim6
    [af__, f_, b_, c, d], -- Abdim??
    [g__, f_, g_, b_, d], -- G7
    [g__, e_, g_, c, e],  -- C
    [g__, d_, g_, c, f],  -- G7sus4
    [g__, d_, g_, b_, f], -- G7
    [g__, ef_, a_, c, fs],-- Adim7
    [g__, e_, g_, c, g],  -- C
    [g__, d_, g_, c, f],  -- G7sus4
    [g__, d_, g_, b_, f], -- G7
    [c__, c_, g_, bf_, e] -- C7
  ]

twice :: [a] -> [a]
twice xs = xs ++ xs

chordToParts :: [Event] -> ([Event], [Event], [Event])
chordToParts (ba:te:trs) = (bass, tenor, treble)
  where
    bass = twice [ba.>2]
    tenor = twice [r.<4, te.>7.<4]
    treble = twice (r.<2 : twice (trs.<<4))

prelude = music $ map (.>>2) [ bass, tenor, treble ]
  where
    (bass, tenor, treble) = foldr (concatParts.chordToParts) ([],[],[]) chords
    concatParts (bs',ts',trs') (bs,ts,trs) = (bs'++bs, ts'++ts, trs'++trs)

main = createMidi "test.midi" $ prelude


--main = createMidi "test.midi" $ music [ [c, d].>>2, [e, f].>>2, [g, a].>>2 ]
--main = createMidi "test.midi" $ music [ [c, d, g, a, c', r, b, a, g, f, d, b_, c.>4] ]


