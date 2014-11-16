import Structure
import Note
import Interval
import Compose
import Midi


data Chord = Chord { bass :: Note, intervals :: [ Interval ] } deriving (Show, Eq)

_M2 = Interval 1 2 0
_m3 = Interval 2 3 0
_M3 = Interval 2 4 0
_P4 = Interval 3 5 0
_d5 = Interval 4 6 0
_P5 = Interval 4 7 0
_m6 = Interval 5 8 0
_M6 = Interval 5 9 0


-- C Major prelude - Book one, well tempered clavier
chords = [
    [c, e, g, c', e'],    -- Chord c [_M3, _P5]                   -- (c, [3, 5])           -- C         -- Tonic
    [c, d, a, d', f'],    -- Chord c [_M2, _P4, _M6]              -- (c, [2, 4, 6])        -- Dm/C
    [b_, d, g, b, f'],    -- Chord b_ [_m3, d5, _m6]              -- (b_, [3, 5, 6])       -- G7/B
    [c, e, g, c', e'],    -- Chord c []   -- (c, [3, 5])           -- C
    [c, e, a, e', a'],    -- Chord c []   -- (c, [3, 6])           -- Am/C
    [c, d, fs, a, d'],    -- Chord c []   -- (c, [2, 4#, 6])       -- D/C      -- Dominant
    [b_, d, g, d', g'],   -- Chord b_ []   -- (b_, [3, 6])         -- G/B
    [a_, c, e, g, c'],    -- Chord a_ []   -- (a_, [3, 5, 7])      -- Am7
    [d_, a_, d, fs, c'],  -- Chord d_ []   -- (d_, [3, 5, 7])      -- D7
    [g_, b_, d, g, b],    -- Chord g_ []   -- (g_, [3, 5])         -- G
    [g_, bf_, e, g, cs'], -- Chord g_ []   -- (g_, [3b, 4s, 6])    -- Gdim7
    [f_, a_, d, a, d'],   -- Chord f_ []   -- (f_, [3, 6])         -- Dm/F
    [f_, af_, d, f, b],   -- Chord f_ []   -- (f_, [3b, 4, 6])     -- Fdim7
    [e_, g_, c, g, c'],   -- Chord e_ []   -- (e_, [3, 6])         -- C/E      -- Tonic
    [e_, f_, a_, c, f],   -- Chord e_ []   -- (e_, [2, 4, 6])      -- F/E
    [d_, f_, a_, c, f],   -- Chord d_ []   -- (d_, [3, 5, 7])      -- Dm7
    [g__, d_, g_, b_, f], -- Chord g__ []   -- (g__, [3, 5, 7])     -- G7
    [c_, e_, g_, c, e],   -- Chord c_ []   -- (c_, [3, 5])         -- C
    [c_, g_, bf_, c, e],  -- Chord c_ []   -- (c_, [3, 5, 7b])     -- C7
    [f__, f_, a_, c, e],  -- Chord f__ []   -- (f__, [3, 5, 7])     -- Fmaj7
    [fs__, c_, a_, c, ef],-- Chord fs__ []   -- (fs__, [3, 5, 7b])   -- Adim7/F#
    [af__, f_, b_, c, d], -- Chord af__ []   -- (af__, [2, 3, 4, 6]) -- Bdim7/Ab
    [g__, f_, g_, b_, d], -- Chord g__ []   -- (g__, [3, 5, 7])     -- G7
    [g__, e_, g_, c, e],  -- Chord g__ []   -- (g__, [4, 6])        -- C/G
    [g__, d_, g_, c, f],  -- Chord g__ []   -- (g__, [4, 5, 7])     -- G7sus4
    [g__, d_, g_, b_, f], -- Chord g__ []   -- (g__, [3, 5, 7])     -- G7
    [g__, ef_, a_, c, fs],-- Chord g__ []   -- (g__, [2, 4, 6f, 7#])-- Adim7/G
    [g__, e_, g_, c, g],  -- Chord g__ []   -- (g__, [4, 6])        -- C/G
    [g__, d_, g_, c, f],  -- Chord g__ []   -- (g__, [4, 5, 7])     -- G7sus4
    [g__, d_, g_, b_, f], -- Chord g__ []   -- (g__, [3, 5, 7])     -- G7
    [c__, c_, g_, bf_, e] -- Chord c__ []   -- (c__, [3, 5, 7f])    -- C7
  ]

coda = (
    [c__, c__, c__].>>4,
    [r.<4, c_.>15.<4, r.<4, c_.>15.<4, c_.>4],
    [r.>2, f_, a_, c, f, c_, a_, c_, a_, f_, a_, f_, d_, f_, d_, r.>2, g, b, d', f', d', b, d', b, g, b, d, f, e, d].<<4 ++ [c'.>4]
  )

-- ! Try analysis...

twice :: [a] -> [a]
twice xs = xs ++ xs

type Parts = ([Event], [Event], [Event])

concatParts :: Parts -> Parts -> Parts
concatParts (b, t, tr) (b', t', tr') = (b++b', t++t', tr++tr')

chordToParts :: [Event] -> Parts
chordToParts (ba:te:trs) = (bass, tenor, treble)
  where
    bass = twice [ba.>2]
    tenor = twice [r.<4, te.>7.<4]
    treble = twice (r.<2 : twice (trs.<<4))

prelude = music $ map (.>>2) [ bass, tenor, treble ]
  where
    (bass, tenor, treble) = concatParts (foldr (concatParts.chordToParts) ([],[],[]) chords) coda
    concatParts (bs',ts',trs') (bs,ts,trs) = (bs'++bs, ts'++ts, trs'++trs)

main = createMidi "test.midi" $ prelude


--main = createMidi "test.midi" $ music [ [c, d].>>2, [e, f].>>2, [g, a].>>2 ]
--main = createMidi "test.midi" $ music [ [c, d, g, a, c', r, b, a, g, f, d, b_, c.>4] ]


