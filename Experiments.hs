import Structure
import Note
import Interval
import Intervals
import Notes
import Midi
import Harmony
import Melody
import Key
import Keys
import Chord
import Numeral
import Numerals
import Data.Ratio


-- Break out things into other modules
-- Next: mechanisms for expanding chords out into parts that obey voice leading rules...


progression = [ _I, _ii7d, _V7b, _I ]

chords :: [[Event]]
chords = map ((map (Play (1%4))) . extend . chordToNotes . (numeralToChord keyOfC)) progression
  where
    extend (a:b:c:[]) = a:b:c:up a:up b:[]
    extend (a:b:c:d:[]) = a:b:c:d:up b:[]
    up = modifyOctave 1



-- C Major prelude - Book one, well tempered clavier
chordsTEMP = [
    [c, e, g, c', e'],    -- C:I         -- (c, [3, 5])          -- C
    [c, d, a, d', f'],    -- ii7d        -- (c, [2, 4, 6])       -- Dm/C
    [b_, d, g, b, f'],    -- V7b         -- (b_, [3, 5, 6])      -- G7/B
    [c, e, g, c', e'],    -- I           -- (c, [3, 5])          -- C
    [c, e, a, e', a'],    -- vib         -- (c, [3, 6])          -- Am/C
    [c, d, fs, a, d'],    -- V7/Vd       -- (c, [2, 4#, 6])      -- D/C
    [b_, d, g, d', g'],   -- G:Ib        -- (b_, [3, 6])         -- G/B
    [a_, c, e, g, c'],    -- ii7         -- (a_, [3, 5, 7])      -- Am7
    [d_, a_, d, fs, c'],  -- V7          -- (d_, [3, 5, 7])      -- D7
    [g_, b_, d, g, b],    -- I           -- (g_, [3, 5])         -- G
    [g_, bf_, e, g, cs'], -- vio7        -- (g_, [3b, 4s, 6])    -- Edim7
    [f_, a_, d, a, d'],   -- iic/IV      -- (f_, [3, 6])         -- Dm/F
    [f_, af_, d, f, b],   -- viio7c/IV   -- (f_, [3b, 4, 6])     -- Bdim7/F
    [e_, g_, c, g, c'],   -- C:Ib        -- (e_, [3, 6])         -- C/E
    [e_, f_, a_, c, f],   -- IVmaj7d     -- (e_, [2, 4, 6])      -- F/E
    [d_, f_, a_, c, f],   -- ii7         -- (d_, [3, 5, 7])      -- Dm7
    [g__, d_, g_, b_, f], -- V7          -- (g__, [3, 5, 7])     -- G7
    [c_, e_, g_, c, e],   -- I           -- (c_, [3, 5])         -- C
    [c_, g_, bf_, c, e],  -- I7          -- (c_, [3, 5, 7b])     -- C7
    [f__, f_, a_, c, e],  -- IVmaj7      -- (f__, [3, 5, 7])     -- Fmaj7
    [fs__, c_, a_, c, ef],-- vio7d       -- (fs__, [3, 5, 7b])   -- Adim7/F#
    [af__, f_, b_, c, d], -- viio7d      -- (af__, [2, 3, 4, 6]) -- Bdim7/Ab
    [g__, f_, g_, b_, d], -- V7          -- (g__, [3, 5, 7])     -- G7
    [g__, e_, g_, c, e],  -- Ic          -- (g__, [4, 6])        -- C/G
    [g__, d_, g_, c, f],  -- V7sus4      -- (g__, [4, 5, 7])     -- G7sus4
    [g__, d_, g_, b_, f], -- V7          -- (g__, [3, 5, 7])     -- G7
    [g__, ef_, a_, c, fs],-- vio7add7    -- (g__, [2, 4, 6f, 7#])-- Adim7/G
    [g__, e_, g_, c, g],  -- Ic          -- (g__, [4, 6])        -- C/G
    [g__, d_, g_, c, f],  -- V7sus4      -- (g__, [4, 5, 7])     -- G7sus4
    [g__, d_, g_, b_, f], -- V7          -- (g__, [3, 5, 7])     -- G7
    [c__, c_, g_, bf_, e] -- I           -- (c__, [3, 5, 7f])    -- C7
  ]

coda = (
    [c__, c__, c__].>>4,
    [r.<4, c_.>15.<4, r.<4, c_.>15.<4, c_.>4],
    [r.>2, f_, a_, c, f, c_, a_, c_, a_, f_, a_, f_, d_, f_, d_, r.>2, g, b, d', f', d', b, d', b, g, b, d, f, e, d].<<4 ++ [c'.>4]
  )

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
    (bass, tenor, treble) = concatParts body coda
    body = foldr (concatParts.chordToParts) ([],[],[]) chords
    concatParts (bs',ts',trs') (bs,ts,trs) = (bs'++bs, ts'++ts, trs'++trs)

main = createMidi "test.midi" $ prelude

--main = putStrLn $ show $ Melody.analyse prelude


--main = createMidi "test.midi" $ music [ [c, d].>>2, [e, f].>>2, [g, a].>>2 ]
--main = createMidi "test.midi" $ music [ [c, d, g, a, c', r, b, a, g, f, d, b_, c.>4] ]


